package laas.fspex.parser

/**
 * Created by Ulrich Matchi Aïvodji on 16/04/2015.
 */



import laas.fspex.model._
import laas.fspex.spp.Dijkstra
import laas.fspex.utils._

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

import scala.collection.mutable

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

import java.io._

import scala.util.Random

object OsmParser {

  def write[WeightedGraph](path:String,o:WeightedGraph) = {
    val file = new FileOutputStream(path)
    val buffer = new BufferedOutputStream(file)
    val output = new ObjectOutputStream(buffer)
    try {
      output.writeObject(o)
    } catch {
      case e:Exception =>
        val f = new File(path)
        if(f.exists) { f.delete }
        throw e
    } finally{
      output.close()
    }
  }
   def read[WeightedGraph](path:String):WeightedGraph = {
    val input = new ObjectInputStream(new FileInputStream(path)) {
      override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
        try { Class.forName(desc.getName, false, getClass.getClassLoader) }
        catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
      }
    }

     input.readObject().asInstanceOf[WeightedGraph]

  }

  def getAttrib(attrs:MetaData, name:String) = {
    val attr = attrs(name)
    if(attr == null) {
      sys.error(s"Expected attribute $name does not exist")
    }
    if(attr.length > 1) {
      sys.error(s"Expected attribute $name has more than one return.")
    }
    attr(0).text
  }

  def parseNode(attrs:MetaData,nodes:mutable.Map[String,Vertex]) = {
    val id = getAttrib(attrs, "id")
    val lat = getAttrib(attrs,"lat").toDouble
    val lon = getAttrib(attrs,"lon").toDouble

    nodes(id) = StreetVertex(Location(lat,lon),id)
  }

  def createEdge(wayNodes:Seq[Vertex],wayInfo:WayInfo,graph:WeightedGraph)={

      wayNodes.reduceLeft { (v1, v2) =>
        graph.addNode(v1)
        graph.addNode(v2)

    //val v1=wayNodes(0)
    //val v2=wayNodes(wayNodes.length-1)

        val n1 = graph.addNode(v1)
        val n2=graph.addNode(v2)

        (n1.connectWith(n2)).setWeight(Distance.distance(v1.location,v2.location))

        (n2.connectWith(n1)).setWeight(Distance.distance(v1.location,v2.location))


        /*if(wayInfo.isWalkable) {
          (n1.connectWith(n2)).setWeight(Distance.distance(v1.location,v2.location))
        }

        if(wayInfo.isBikable) {
          wayInfo.direction match {
            case OneWay =>
              (n1.connectWith(n2)).setWeight(Distance.distance(v1.location,v2.location))
            case OneWayReverse =>
              (n2.connectWith(n1)).setWeight(Distance.distance(v1.location,v2.location))
            case BothWays =>
              (n1.connectWith(n2)).setWeight(Distance.distance(v1.location,v2.location))
              (n2.connectWith(n1)).setWeight(Distance.distance(v1.location,v2.location))

          }
        }

        if(wayInfo.isDrivable) {

          wayInfo.direction match {
            case OneWay =>
              (n1.connectWith(n2)).setWeight(Distance.distance(v1.location,v2.location))
            case OneWayReverse =>
              (n2.connectWith(n1)).setWeight(Distance.distance(v1.location,v2.location))
            case BothWays =>
              (n1.connectWith(n2)).setWeight(Distance.distance(v1.location,v2.location))
              (n2.connectWith(n1)).setWeight(Distance.distance(v1.location,v2.location))
          }
        }*/


        v2
      }
    }


  def parseRoad(parser:XMLEventReader,
               wayAttribs:MetaData,
               nodes:mutable.Map[String,Vertex],
                roads:mutable.Map[String,Road],
                graph:WeightedGraph
               )= {
    val wayNodes = mutable.ListBuffer[Vertex]()
    var break = !parser.hasNext
    var wayInfo:WayInfo = null

    val wayId = getAttrib(wayAttribs,"id")

    val tags = mutable.Map[String,String]()

    while(!break) {
      parser.next match {
        case EvElemStart(_,"nd",attrs,_) =>
          val id = getAttrib(attrs,"ref")
          if(nodes.contains(id)) {
            val v=nodes(id)
            wayNodes += v
          }
        case EvElemStart(_,"tag",attrs,_) =>
          val k = getAttrib(attrs,"k")
          val v = getAttrib(attrs,"v")
          tags(k) = v
        case EvElemEnd(_,"way") => wayInfo = WayInfo.fromTags(wayId,tags.toMap)

          if(wayInfo.isDrivable) {

            createEdge(wayNodes,wayInfo,graph)

            var name="Rue sans nom"
            if (wayInfo.tags.contains("name")){
              name=wayInfo.tags("name")
            }

            var distance=0.0
            wayNodes.reduceLeft { (v1,v2) =>
              distance+=Distance.distance(v1.location,v2.location)
              v2}

            roads(wayId)=Road(name,wayInfo.tags("highway"),wayNodes(0),wayNodes(wayNodes.length-1),distance,wayNodes)
          }
          break = true
        case _ => // pass
      }
      break = break || !parser.hasNext
    }

  }



  def vrp(osmPath:String, save:String,numb:Int)= {
    val nodes = mutable.Map[String,Vertex]()
    val roads= mutable.Map[String,Road]()
    val graph = new WeightedGraph(100.0)

    Logger.timed("Parsing OSM XML into nodes and roads...",
      "OSM XML parsing complete.") { () =>
      val source = Source.fromFile(osmPath)

      try {
        val parser = new XMLEventReader(source)
        while(parser.hasNext) {
          parser.next match {

            case EvElemStart(_,"node",attrs,_) =>
              parseNode(attrs,nodes)
            case EvElemStart(_,"way",attrs,_) => parseRoad(parser,attrs,nodes,roads,graph)

            case _ => //pass
          }
        }
      } finally {
        source.close
      }
    }

    write(save,graph)

    val writer = new PrintWriter(new File(save))

    var stations=mutable.ListBuffer[WeightedGraph#Node]()


    val gen =Random

    for(i<-1 to numb){
      stations+=graph.nodes(gen.nextInt(graph.nodes.length))
    }


    writer.write("id || name ||start|| end || type || distance(m)\n")



    for(station_i<-stations){

      for(station_j<-stations){

        val (source,target)=(station_i,station_j)

          if (source != target) {

            val dijkstra = new Dijkstra[graph.type](graph)

            // Halt when target is scanned true for normal dij
            dijkstra.stopCondition = (S, D, P) => !S.contains(target)


            val (distance, path) = dijkstra.compute(source)

            var dis = -1.0

            if (distance.contains(target)) {
              dis = distance(target)
            }

            writer.write("road" + "||" + "road.name" + "||" + source + "||" + target + "||" + "road.highway" + "||" + dis + "\n")

          }


      }

    }

    writer.close()

    /*val y=roads.head
    val (source,target)=(graph.no, y._2.wayNodes(y._2.wayNodes.length-1))
    val dijkstra = new Dijkstra[graph.type](graph)

    // Halt when target is scanned true for normal dij
    dijkstra.stopCondition = (S, D, P) => !S.contains(target)


    val (distance, path) = dijkstra.compute(source)*/





  }



  def readData(path:String)={


    val bb:WeightedGraph=read(path)

    println(bb.edges.length)

  }
}
