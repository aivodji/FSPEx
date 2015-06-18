package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */
object Distance {
  val radiusOfEarth = 6371010 // Meters
  val globalMinLat = math.toRadians(-90.0)
  val globalMaxLat = math.toRadians(90.0)
  val globalMinLong = math.toRadians(-180.0)
  val globalMaxLong = math.toRadians(180.0)

  val maxLatDelta = math.toRadians(4.0)
  val maxLongDelta = math.toRadians(4.0)

  val maxErrorInverse = 0.999462

  def degToRad(deg:Double) = { deg * (math.Pi / 180.0) }

  def distance(lat1:Double,long1:Double,lat2:Double,long2:Double):Double = {
    // See http://www.movable-type.co.uk/scripts/latlong.html
    val dLat = math.toRadians(lat2-lat1)
    val dLon = math.toRadians(long2-long1)

    if(dLat > maxLatDelta || dLon > maxLongDelta) {
      // Distance is too great for fast equilateral projection

      val rlat1 = math.toRadians(lat1)
      val rlat2 = math.toRadians(lat2)

      val x = math.sin(dLat/2)
      val y = math.sin(dLon/2)
      val a = x * x + y * y * math.cos(rlat1) * math.cos(rlat2)
      radiusOfEarth * 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
    } else {
      // Fast equilateral projection
      val x = dLon * Math.cos(math.toRadians((lat1+lat2)/2))
      radiusOfEarth * math.sqrt(dLat*dLat + x*x) * maxErrorInverse
    }
  }

  def distance(p1:Location, p2:Location):Double =
    distance(p1.lat,p1.long,p2.lat,p2.long)

  def toFeet(meters:Double) =
    meters * 3.28084


}

