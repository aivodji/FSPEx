package laas.fspex.parser

import laas.fspex.model.Speeds
import laas.fspex.model.Node

/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */

trait WayInfo {
  val wayId:String

  def isWalkable:Boolean
  def walkSpeed:Double

  val isBikable:Boolean
  val bikeSpeed:Double

  val isDrivable:Boolean
  val carSpeed:Double

  private var _direction:WayDirection = BothWays
  def direction = _direction

  val tags:Map[String,String]
}

abstract sealed class WayDirection
case object OneWay extends WayDirection
case object BothWays extends WayDirection
case object OneWayReverse extends WayDirection

case object Impassable extends WayInfo {
  val wayId = "IMPASSABLE"
  val isWalkable = false
  val isBikable = false
  val isDrivable=false

  val walkSpeed = 0.0
  val bikeSpeed = 0.0
  val carSpeed = 0.0
  val tags = Map[String,String]()
}

trait Walkable {
  val isWalkable = true

  val walkSpeed = Speeds.walking
}

trait Bikable {
  val isBikable = true

  val bikeSpeed = Speeds.biking
}

trait Drivable {
  val isDrivable = true
  val carSpeed = Speeds.driving
}

case class WalkOrBike(wayId:String,tags:Map[String,String]) extends WayInfo
with Walkable
with Bikable{
  val isDrivable =false
  val carSpeed= 0.0
}

case class WalkOrCar(wayId:String,tags:Map[String,String]) extends WayInfo
with Walkable
with Drivable {
  val isBikable = false
  val bikeSpeed = 0.0
}

case class WalkOrBikeOrCar(wayId:String,tags:Map[String,String]) extends WayInfo
with Walkable
with Bikable
with Drivable


case class WalkOnly(wayId:String,tags:Map[String,String]) extends WayInfo
with Walkable {
  val isBikable = false
  val isDrivable = false
  val bikeSpeed = 0.0
  val carSpeed = 0.0
}

case class BikeOnly(wayId:String,tags:Map[String,String]) extends WayInfo
with Bikable {

  val isWalkable = false
  val isDrivable = false
  val walkSpeed = 0.0
  val carSpeed = 0.0
}

case class CarOnly(wayId:String,tags:Map[String,String]) extends WayInfo
with Drivable {
  val isWalkable = false
  val isBikable = false
  val walkSpeed = 0.0
  val bikeSpeed = 0.0
}

object WayInfo {
  // http://wiki.openstreetmap.org/wiki/Key:oneway
  private val oneWayTrueValues = Set("yes","true","1")
  private val oneWayReverseValues = Set("-1","reverse")

  def fromTags(wayId:String,tags:Map[String,String]):WayInfo = {
    var info:WayInfo = null

    if(tags.contains("highway")) {
      info = forHighwayType(wayId,tags)
    }

    if(info == null) {
      if(tags.contains("public_transport")) {
        if(tags("public_transport") == "platform") {
          info = WalkOnly(wayId,tags)
        }
      }
    }

    if(info == null) {
      if(tags.contains("railway")) {
        if(tags("railway") == "platform") {
          info = WalkOnly(wayId,tags)
        }
      }
    }

    info match {
      case null => Impassable
      case Impassable => Impassable
      case _ =>
        // Check for one-way
        if(tags.contains("oneway")) {
          val oneway = tags("oneway")
          info._direction =
            if(oneWayTrueValues.contains(oneway)) {
              OneWay
            } else if (oneWayReverseValues.contains(oneway)) {
              OneWayReverse
            } else {
              BothWays
            }
        }
        info
    }
  }

  // http://wiki.openstreetmap.org/wiki/Map_Features#Highway
  def forHighwayType(wayId:String,tags:Map[String,String]):WayInfo =
    tags("highway") match {

      case "motorway" => CarOnly(wayId,tags)
      case "motorway_link" => CarOnly(wayId,tags)
      case "trunk" => CarOnly(wayId,tags)
      case "trunk_link" => CarOnly(wayId,tags)
      case "primary" => CarOnly(wayId,tags)
      case "primary_link" => CarOnly(wayId,tags)
      case "secondary" => CarOnly(wayId,tags)
      case "secondary_link" => CarOnly(wayId,tags)
      case "tertiary" => CarOnly(wayId,tags)
      case "tertiary_link" => CarOnly(wayId,tags)
      case "living_street" => CarOnly(wayId,tags)
      case "residential" => CarOnly(wayId,tags)
      case "unclassified" => CarOnly(wayId,tags)
      case "track" => CarOnly(wayId,tags)
      case "road" => CarOnly(wayId,tags)


      case "pedestrian" => CarOnly(wayId,tags)
      case "service" => CarOnly(wayId,tags)
      case "bus_guideway" => CarOnly(wayId,tags)
      case "raceway" => CarOnly(wayId,tags)
      case "path" => CarOnly(wayId,tags)
      case "footway" => CarOnly(wayId,tags)
      case "cycleway" => CarOnly(wayId,tags)
      case "bridleway" => CarOnly(wayId,tags)
      case "steps" => CarOnly(wayId,tags)
      case "proposed" => CarOnly(wayId,tags)
      case "construction" => CarOnly(wayId,tags)
      case "bus_stop" => CarOnly(wayId,tags)
      case "crossing" => CarOnly(wayId,tags)
      case "emergency_access_point" => CarOnly(wayId,tags)
      case "escape" => CarOnly(wayId,tags)
      case "give_way" => CarOnly(wayId,tags)
      case "mini_roundabout" => CarOnly(wayId,tags)
      case "parking" => CarOnly(wayId,tags)

      case _ => Impassable
    }
}