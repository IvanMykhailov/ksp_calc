package stridervan.ksp.calc

import Const._

object PartSize extends Enumeration {
  type PartSize = Value
  val u1, u2, u3, u4, u8, u0_5 = Value
}
import PartSize._

trait RocketPart {
  def name: String
  def mass: Double
  def diameter: PartSize.Value
  def length: PartSize.Value
}

case class Load(
  name: String,
  mass: Double, 
  length: PartSize.Value = u1, 
  diameter: PartSize.Value = u1
) extends RocketPart

case class FuelTank(
  name: String,
  totalMass: Double,
  dryMass: Double, 
  length: PartSize.Value, 
  diameter: PartSize.Value
) extends RocketPart {
  override val mass = totalMass
}

object ContainFuel {
  def unapply(part: RocketPart): Option[Double] = part match {
    case FuelTank(_, fullMass, emptyMass, _, _) => Some(fullMass - emptyMass)
    case _ => None
  }  
}

case class Engine (
  name: String,
  mass: Double, 
  length: PartSize, 
  diameter: PartSize,
  thrust: Double,
  ispSea: Double,
  ispSpace: Double
) extends RocketPart

object Thrust {
  def unapply(part: RocketPart): Option[Double] = part match {
    case Engine(_, _, _, _, thrust, ispSea, ispSpace) => Some(thrust)
    case _ => None
  }
}

object IsEngine {
  def unapply(part: RocketPart): Option[(Double, Double, Double)] = part match {
    case Engine(_, _, _, _, thrust, ispSea, ispSpace) => Some((thrust, ispSea, ispSpace))
    case _ => None
  }
} 

case class Stage(number: Int, parts: Map[RocketPart, Int]) {
  
  val totalMass = parts.map {
    case (part, count) => part.mass * count
  }.sum
    
  
  val fuelMass = parts.map {
    case (ContainFuel(fuel), count) => fuel * count
    case _ => 0
  }.sum
      
  val dryMass: Double = totalMass - fuelMass
    
  
  val (thrust, ispSea, ispSpace) =
  {
    var totalThrust = 0D
    var consumtionSea = 0D
    var consumtionSpace = 0D
    
    parts.foreach {
      case (IsEngine(thrust, ispSea, ispSpace), count) => 
        totalThrust += thrust * count
        consumtionSea += (thrust / ispSea) * count
        consumtionSpace += (thrust / ispSpace) * count
      case _ =>
    }
    ( totalThrust, 
      if (consumtionSea == 0) 0 else totalThrust / consumtionSea, 
      if (consumtionSpace == 0) 0 else totalThrust / consumtionSpace)
  }
  
 
  def thrustToWeight(massOnTop: Double) = {
    val weight = (totalMass + massOnTop) * Const.gKerbal
    if (weight == 0) 0
    else thrust / weight
  }
   
  def deltaVsea(massOnTop: Double) = 
    if (totalMass == 0) 0 
    else ispSea * gKerbal * Math.log((totalMass + massOnTop) / (dryMass + massOnTop))
    
  def deltaVspace(massOnTop: Double) = 
    if (totalMass == 0) 0 
    else ispSpace * gKerbal * Math.log((totalMass + massOnTop) / (dryMass + massOnTop))
}