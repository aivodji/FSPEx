package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 16/04/2015.
 */
import scala.collection.mutable

case class Location(lat:Double,long:Double)
object Location {
  implicit def location2Tuple(l:Location):(Double,Double) =
    (l.lat,l.long)

  def apply(tup:(Double,Double)):Location = Location(tup._1,tup._2)
}
