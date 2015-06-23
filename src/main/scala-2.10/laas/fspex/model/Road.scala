package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 20/06/2015.
 */

import scala.collection.mutable._

case class Road(name:String, highway:String, begin:Vertex, end:Vertex, distance:Double, wayNodes:ListBuffer[Vertex]) {


override
  def toString = {
    s"Road($name,$highway,$begin,$end,$distance)"
  }
}


case class vrpRoad(id:String,name:String, highway:String, begin:Vertex, end:Vertex, distance:Double) {
  override
  def toString = {
    s"vrpRoad($id,$name,$highway,$begin,$end,$distance)"
  }
}