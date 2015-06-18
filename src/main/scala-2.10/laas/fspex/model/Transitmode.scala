package laas.fspex.model


/**
 * Created by Ulrich Matchi Aïvodji on 18/06/2015.
 */
abstract sealed class TransitMode extends Serializable
{ val isTimeDependant:Boolean }

case object Walking extends TransitMode {
  val isTimeDependant = false

  override
  def toString = "Walking"
}
case object Biking extends TransitMode {
  val isTimeDependant = false

  override
  def toString = "Biking"
}

case object Driving extends TransitMode {
  val isTimeDependant = false

  override
  def toString = "Driving"
}

