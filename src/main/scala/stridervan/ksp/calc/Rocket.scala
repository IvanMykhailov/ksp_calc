package stridervan.ksp.calc

case class Rocket (stages: Seq[Stage]) {
  
  val mass = totalMass(stages)
  
  lazy val firstStageTwr = stages.head.thrustToWeight(massOnTop = upperStagesMass)
  lazy val firstStageDeltaV = ((stages.head.deltaVsea(upperStagesMass) + stages.head.deltaVspace(upperStagesMass)) / 2)
  
  lazy val deltaV_space = _deltaV_space(stages.toList)
  //println("Total dV: " + deltaV_space)
  
  lazy val deltaV_fromGround = firstStageDeltaV + _deltaV_space(stages.tail.toList)
  
  private def totalMass(stages: Seq[Stage]) = stages.map(_.totalMass).sum
  
  private lazy val upperStagesMass = totalMass(stages.tail)
  
  private def _deltaV_space(stages: List[Stage]): Double = stages match {
    case stage :: tail =>
      val massOnTop = totalMass(tail)
      /*
      println(s"------------- Stage ${stage.number} -------------")
      println(s"Mass on top: " + massOnTop)
      println(s"Mass:        " + stage.totalMass)
      println(s"Dry mass:    " + stage.dryMass)
      println(s"delta V:     " + stage.deltaVspace(totalMass(tail)))
      println(s"Thrust:      " + stage.thrust)
      println(s"Isp_space:   " + stage.ispSpace)
      println(s"TWR:         " + stage.thrust / ((massOnTop + stage.totalMass) * Const.gKerbal))
      println()
      */
      stage.deltaVspace(totalMass(tail)) + _deltaV_space(tail)
    case Nil => 0
  }
}