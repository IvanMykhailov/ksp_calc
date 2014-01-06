package stridervan.ksp.calc

import PartSize._

object parts {
  
  def mapToSeq(parts: Map[RocketPart, Int]) = 
    parts.flatMap {
      case (part, count) => Seq.fill(count)(part) 
    }.toSeq
    
  def seqToMap(parts: Seq[RocketPart]) = 
    parts.foldLeft(Map.empty[RocketPart, Int]) {
      case (map, part) =>
        if (map.contains(part)) {
          map + (part -> (map(part) + 1))
        } else {
          map + (part -> 1)
        }      
    }
  
  def byName(name: String) = {
    val part = parts.filter(_.name == name)
    part.size match {
      case 0 => throw new Exception("No part with name: " + name)
      case 1 => part(0)
      case _ => throw new Exception("Several partf with name: " + name)
    }
  }
  
  val separator = Load(
    name = "TR-18A (Decoupler 1 unit diameter)",
    mass = 0.05, 
    length = u0_5, 
    diameter = u1
  )
  
  val tanks = Seq(
    FuelTank(
      name = "FL-T100",
      totalMass = 0.5625,
      dryMass = 0.0625, 
      length = u0_5, 
      diameter = u1
    )/*,
    
    FuelTank(
      name = "FL-T200",
      totalMass = 1.125,
      dryMass = 0.125, 
      length = u1, 
      diameter = u1
    ),
    
    FuelTank(
      name = "FL-T400",
      totalMass = 2.25,
      dryMass = 0.25, 
      length = u2, 
      diameter = u1
    ),
    
    FuelTank(
      name = "FL-T800",
      totalMass = 4.5,
      dryMass = 0.5, 
      length = u4, 
      diameter = u1
    ),
    
    /*
    FuelTank(
      name = "X200-8",
      totalMass = 4.5,
      dryMass = 0.5, 
      length = u1, 
      diameter = u2
    ),
    */
    
    FuelTank(
      name = "X200-16",
      totalMass = 9,
      dryMass = 1, 
      length = u2, 
      diameter = u2
    ),
     
    FuelTank(
      name = "X200-32",
      totalMass = 18,
      dryMass = 2, 
      length = u4, 
      diameter = u2
    ),
    
    FuelTank(
      name = "Jumbo64",
      totalMass = 36,
      dryMass = 4, 
      length = u8, 
      diameter = u2
    ),
    
    FuelTank(
      name = "Round8 Toroidal",
      totalMass = 0.136,
      dryMass = 0.025, 
      length = u0_5, 
      diameter = u0_5
    )*/
  )
  
  val engines = Seq(
    Engine (
      name = "LV-T30",
      mass = 1.25, 
      length = u2, 
      diameter = u1,
      thrust = 215,
      ispSea = 320,
      ispSpace = 370
    ),
  
    /*
    Engine (
      name = "LV-T45",
      mass = 1.5, 
      length = u2, 
      diameter = u1,
      thrust = 200,
      ispSea = 320,
      ispSpace = 370
    ),
    */
    
    Engine (
      name = "LV-909",
      mass = 0.5, 
      length = u1, 
      diameter = u1,
      thrust = 50,
      ispSea = 300,
      ispSpace = 390
    ),
    
    
    Engine (
      name = "48-7S",
      mass = 0.1, 
      length = u0_5, 
      diameter = u0_5,
      thrust = 20,
      ispSea = 300,
      ispSpace = 350
    ),
    
    
    Engine (
      name = "Skipper",
      mass = 4, 
      length = u3, 
      diameter = u2,
      thrust = 650,
      ispSea = 300,
      ispSpace = 350
    )
  
  )

  
  val parts = tanks ++ engines
}