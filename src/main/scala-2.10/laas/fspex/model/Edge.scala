package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */


sealed abstract class EdgeType

case object WalkEdge extends EdgeType {
  def apply(target:Vertex,travelTime:Duration) =
    Edge(target,Time.ANY,travelTime,Walking)
}

case object BikeEdge extends EdgeType {
  def apply(target:Vertex,travelTime:Duration) =
    Edge(target,Time.ANY,travelTime,Biking)
}

case object DriveEdge extends EdgeType {
  def apply(target:Vertex,travelTime:Duration) =
    Edge(target,Time.ANY,travelTime,Driving)
}

case class Edge(target:Vertex,
                time:Time,
                travelTime:Duration,
                mode:TransitMode) {
  def isAnyTime = time.isAny
}


