package stridervan.ksp.calc

object FitnessFunction {
  
  def toKerbalOrbit(rocket: Rocket): Boolean = {    
    (rocket.firstStageTwr > 1.2) &&
    (rocket.firstStageDeltaV > 2000) &&
    (rocket.deltaV_fromGround > 4500)
  }
}