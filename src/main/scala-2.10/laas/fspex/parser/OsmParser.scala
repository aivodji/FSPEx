package laas.fspex.parser

/**
 * Created by Ulrich Matchi Aïvodji on 16/04/2015.
 */

import laas.fspex.model._
import laas.fspex.utils._

import scala.collection.mutable
import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull._

object OsmParser {

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

  def parseNode(attrs:MetaData, pois:mutable.Map[String,Poi]) = {
    val id = getAttrib(attrs, "id")
    val lat = getAttrib(attrs,"lat").toDouble
    val lon = getAttrib(attrs,"lon").toDouble
    val name=""

    pois(id) = SimplePoi(Location(lat,lon),id,name)

    println("node: " + SimplePoi(Location(lat,lon),id,name))
  }

  def parseTag(attrs:MetaData) = {

    val value = getAttrib(attrs, "v")
    val key = getAttrib(attrs,"k")

    println("tags: " + value + key)
  }





  def parse(osmPath:String) = {
    val nodes = mutable.Map[String,Poi]()

    Logger.timed("Parsing OSM XML into nodes...",
      "OSM XML parsing complete.") { () =>
      val source = Source.fromFile(osmPath)

      try {
        val parser = new XMLEventReader(source)
        while(parser.hasNext) {
          parser.next match {
            case EvElemStart(_,"node",attrs,_) =>
              parseNode(attrs,nodes)

            case EvElemStart(_,"tag",attrs,_) =>
              parseTag(attrs)

            case _ => //pass
          }
        }
      } finally {
        source.close
      }
    }

    Logger.log(s"OSM File contains ${nodes.size} nodes")
  }


}
