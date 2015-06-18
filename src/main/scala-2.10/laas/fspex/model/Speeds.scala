package laas.fspex.model

/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */
object Speeds {
  val walking = 1.33 // m/s
  // biking speed based on http://arxiv.org/abs/1011.6266
  val biking = 3.2  // m/s

  val driving = 10.0 // m/s

  val motorway = 36.11
  val trunk = 30.55
  val primary = 25.0
  val secondary = 25.0
  val tertiary = 25.0
  val unclassified = 25.0
  val residential = 25.0
  val livingstreet = 5.55

}
