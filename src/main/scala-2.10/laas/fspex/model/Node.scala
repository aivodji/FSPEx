package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */
import scala.collection.mutable

case class Node(id:String,lat:Double,long:Double) {
  val outgoingEdges = mutable.ListBuffer[Node]()

  override
  def hashCode = id.hashCode

  override
  def equals(other: Any) =
    other match {
      case that: Node => this.id == that.id
      case _ => false
    }

  def addEdgeTo(node:Node) =
    outgoingEdges += node
}

