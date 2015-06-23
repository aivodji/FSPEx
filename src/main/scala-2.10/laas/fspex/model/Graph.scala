package laas.fspex.model

import laas.fspex.parser.WayInfo

import scala.collection.mutable

import java.io._

/**
 * Created by Ulrich Matchi Aïvodji on 22/06/2015.
 */


/*class Graph(){

  var nodes=mutable.ListBuffer[Vertex]()
  var edges=mutable.ListBuffer[Edge]()

  def addNode(v:Vertex)={
    if(!nodes.contains(v)){
      nodes+=v
      //println("yolo")
    }
  }

  def addEdge(a:Vertex, b:Vertex, wayInfo:WayInfo)={
    //println("hi")
    edges+=Edge(wayInfo.tags("highway"),a,b,Distance.distance(a.location,b.location))

    /*
    wayInfo.direction match {
      case OneWay =>
        edges=Edge(wayInfo.tags("highway"),a,b,Distance.distance(a.location,b.location))::edges
      case OneWayReverse =>
        edges=Edge(wayInfo.tags("highway"),a,b,Distance.distance(b.location,a.location))::edges
      case BothWays =>
        edges=Edge(wayInfo.tags("highway"),a,b,Distance.distance(b.location,a.location))::edges
        edges=Edge(wayInfo.tags("highway"),a,b,Distance.distance(a.location,b.location))::edges
    }
     */
  }

}*/

abstract class lGraph{
  type Edge   <: IEdge
  type Node  <: INode
  abstract class INode(vertex:Vertex) {
    def connectWith(node: Node): Edge
  }
  abstract class IEdge {
    def a: Node
    def b: Node
    def opposite(n: Node): Option[Node]
  }
  def nodes: List[Node]
  def edges: List[Edge]
  def addNode(v:Vertex): Node
}

abstract class UndirectedGraph extends lGraph {

  class EdgeImpl(one: Node, other: Node) extends IEdge {
    def a = one
    def b = other
    def opposite(n: Node): Option[Node] =
      if(n == a) Some(b)
      else if(n == b) Some(a)
      else None
  }

  class NodeImpl(v:Vertex) extends INode(v) {
    this: Node =>
    def connectWith(node: Node): Edge = {
      val edge = newEdge(this, node)
      edges = edge :: edges
      edge
    }
    override def toString:String =this.v.name

  }

  protected def newNode(v:Vertex): Node
  protected def newEdge(one: Node, other: Node): Edge

  var nodes: List[Node] = Nil
  var edges: List[Edge] = Nil

  def addNode(v:Vertex): Node = {
    val node = newNode(v)
    nodes = node :: nodes
    node
  }
}

/*class ConcreteUndirectedGraph extends UndirectedGraph {

  type Node = NodeImpl
  type Edge = EdgeImpl
  protected def newNode(v:Vertex): Node = new Node(v)
  protected def newEdge(one: Node, other: Node): Edge =
    new Edge(one, other)
  override    def addNode(v:Vertex): Node = {
    val node = newNode(v)
    nodes = node :: nodes
    node
  }

}*/
@SerialVersionUID(2340L)
class WeightedGraph(defaultWeight: Double) extends UndirectedGraph with Serializable {

  type Node = NodeImpl
  type Edge = EdgeImpl with Weight

  trait Weight {
    var weight = defaultWeight
    def getWeight = weight
    def setWeight(weight: Double): Unit = {
      this.weight = weight
    }
  }
  override protected def newNode(v:Vertex): Node = new NodeImpl(v)
  override protected def newEdge(one: Node, other: Node): Edge with Weight =
    new EdgeImpl(one, other) with Weight

  override   def addNode(v:Vertex): Node = {
    val node = newNode(v)
    nodes = node :: nodes
    node
  }
}