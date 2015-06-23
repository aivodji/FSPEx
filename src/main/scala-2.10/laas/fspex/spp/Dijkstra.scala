package laas.fspex.spp

/**
 * Created by Ulrich Matchi Aïvodji on 23/06/2015.
 */

import laas.fspex.model.WeightedGraph

import scala.collection.mutable._


class Dijkstra[G <: WeightedGraph](graph: G) {
  type Node = WeightedGraph#Node
  type Edge = WeightedGraph#Node

  //stop condition for the algogithm
  type StopCondition = (Set[Node], Map[Node, Double], Map[Node, Node])
    => Boolean


  val defaultStopCondition: StopCondition = (_, _, _) => true
  var stopCondition = defaultStopCondition

  def compute(start: Node): (Map[Node, Double], Map[Node, Node]) = {

    /*object VertexOrdering extends Ordering[Node] {
      def compare(a:Node, b:Node) =
        -(distance(a) compare distance(b))
    }

    val queue = PriorityQueue[Int]()(VertexOrdering)*/

    var queue: Set[Node] = new HashSet()
    var settled: Set[Node] = new HashSet()
    var distance: Map[Node, Double] = new HashMap()

    /*for(node<-graph.nodes){
      distance(node) = -1
    }*/

    var path: Map[Node, Node] = new HashMap()
    queue += start


    distance(start) = 0.0

    //println("------------------------------cost origin: "+distance(start))

    while(!queue.isEmpty && stopCondition(settled, distance, path)) {
      val u = extractMinimum(queue, distance)
      settled += u
      relaxNeighbors(u, queue, settled, distance, path)
    }

    return (distance, path)
  }

  /**
   * Finds element of <code>Q</code> with minimum value in D, removes it
   * from Q and returns it.
   */
  protected def extractMinimum[T](Q: Set[T], D: Map[T, Double]): T = {
    var u = Q.head
    Q.foreach((node) =>  if(D(u) > D(node)) u = node)
    Q -= u
    return u;
  }

  //relaxation
  protected def relaxNeighbors(u: Node, Q: Set[Node], S: Set[Node],
                               D: Map[Node, Double], P: Map[Node, Node]): Unit = {
    for(edge <- graph.edges if(edge.a == u || edge.b == u) ) {
      var v = if(edge.a == u) edge.b else edge.a
      if(!S.contains(v)) {
        if(!D.contains(v) || D(v) > D(u) + edge.getWeight) {
          //println("------------------------------cost v before: "+D(v))
          D(v) = D(u) + edge.getWeight
          //println("------------------------------cost v after: "+D(v))
          P(v) = u
          Q += v
        }
      }
    }

  }
}