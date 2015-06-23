package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 22/06/2015.
 */
case class Edge(highway:String, begin:Vertex, end:Vertex, distance:Double) {
  override
  def toString = {
    s"Edge($highway,$begin,$end,$distance)"
  }
}
