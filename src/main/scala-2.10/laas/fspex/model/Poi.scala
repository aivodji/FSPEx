package laas.fspex.model

/**
 * Created by administrateur on 16/04/2015.
 */

sealed abstract class PoiType

case object SimplePoi extends PoiType {
  def apply(location:Location,id:String, name:String) = Poi(location,id,name,SimplePoi)
}

case object OtherPoi extends PoiType {
  def apply(location:Location, id:String, name:String) = Poi(location,id,name,OtherPoi)
}

case class Poi(location:Location, id:String, name:String, poiType:PoiType) {
  override
  def toString = {
    s"V($id,$location)"
  }

  override
  def hashCode = location.hashCode

  override
  def equals(other: Any) =
    other match {
      case that: Poi => this.location == that.location
      case _ => false
    }
}
