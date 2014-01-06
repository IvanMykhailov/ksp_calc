package test

import stridervan.ksp.calc._
import org.specs2.mutable._
import scala.collection.mutable
import scala.compat.Platform
import org.specs2.matcher.BeEqualTo



class RocketTest extends SpecificationWithJUnit {

  
  "Rocket" should {
    /*
    "not fly without engine" in {
      val stage1 = Stage(number = 1, parts = Seq())
      stage1.thrust must beEqualTo(0)
      stage1.ispSpace must beEqualTo(0)
    
      val rocket = Rocket(Seq(stage1))      
      rocket.deltaV_space must beEqualTo(0)
      
    }
    */
    /*
    "fly if have fuel and engine" in {
      import PartSize._
      val stage1 = Stage(number = 1, parts = Seq(
        Load(
         "Pod Mk1",
         mass = 0.8, 
         length = u1, 
         diameter = u1
        ), 
        
        parts.byName("FL-T400"),
        parts.byName("FL-T400"),
        parts.byName("FL-T400"),
        parts.byName("FL-T400"),
        parts.byName("LV-T30")
      ))
      
      stage1.thrust must beCloseTo(215, 0.001)
      stage1.ispSpace must beCloseTo(370, 0.001)
 
      val rocket = Rocket(Seq(stage1))
  
      rocket.deltaV_space must beCloseTo(4500D, 200)
    }
    
    "fly with 2 stages" in {
      import PartSize._
      val stage3 = Stage(number = 3, parts = Seq(
        Load(
         "Pod Mk1",
         mass = 0.8, 
         length = u1, 
         diameter = u1
        ), 
        
        parts.byName("FL-T200"),
        parts.byName("48-7S")
      ))
      
      val stage2 = Stage(number = 2, parts = Seq(
        parts.separator,
        parts.byName("FL-T400"),
        parts.byName("LV-909")
      )) 
      
      val stage1 = Stage(number = 1, parts = Seq(
        parts.separator,
        parts.byName("FL-T800"),
        parts.byName("FL-T400"),
        parts.byName("LV-T30")
      )) 
      
      stage1.thrust must beCloseTo(215, 0.001)
      stage1.ispSpace must beCloseTo(370, 0.001)
 
      val rocket = Rocket(Seq(stage1, stage2, stage3))
  
      rocket.deltaV_space must beCloseTo(4600D, 100)
    }
    * 
    */
    
    "t" in {
      val n = 4  
      val t = for {
        p1 <- (0 to n).toStream
        p2 <- (0 to (n - p1)).toStream
        p3 <- (0 to (n - p1 - p2)).toStream
        p4 <- (0 to (n - p1 - p2 - p3)).toStream
      } yield {
        //println("inside")
        (p1, p2, p3, p4, n - p1 - p2 -p3 - p4)
      }
    
        
      def next(depthLeft: Int, limit: Int, prefix: Seq[Int]): Stream[Seq[Int]] = {
        if (depthLeft == 0) {
          //println("inside: " + (prefix :+ limit))
          Stream(prefix :+ limit)
        } else {
          (0 to limit).toStream.flatMap { count =>
            next(depthLeft - 1,limit - count, prefix :+ count)
          }  
        }
      }
      
      def combinations(n: Int, positions: Int) = next(positions, n, Seq.empty)
            
      
      val t3 = combinations(n, 5 - 1)
      t3.length should beEqualTo(t.length)
      
      /*
      nextState(Seq(1,2,0,0), 3) must beEqualTo(Some(Seq(0,3,0,0)))
      nextState(Seq(1,1,0,1), 3) must beEqualTo(Some(Seq(2,1,0,0)))
      nextState(Seq(1,0,0,2), 3) must beEqualTo(Some(Seq(2,0,0,1)))
      nextState(Seq(0,0,3,0), 3) must beEqualTo(None)
      */
      
      
      val i  = new CombinationIterator(parts.engines.size, 10).toIterable
      val im = new CombinationIteratorM(parts.engines.size, 10).toIterable      
      i must beEqualTo(im)
      
      (new CombinationIterator(5, n).toIterable).size should beEqualTo(t.length)
      
      /*
      val startT = Platform.currentTime
      for (i <- 1 to 50) {
        val start = Platform.currentTime
        //val iter = new CombinationIteratorM(parts.engines.size, i).toIterable
        val iter = Combinator.partCombinations(parts.engines, i)
        val size = iter.size
        for (p1 <- iter) {
          if (p1.size == 0) println(p1) 
        }
        val stop = Platform.currentTime
        println(s"$i -> $size (millis: ${stop - start})")
      }
      val stopT = Platform.currentTime
      println(s"Total: ${stopT - startT}")
      */
      
      success
    }
    
    "be calculated for 1 stage" in {
      val startT = Platform.currentTime
      
          
      val rocket = Combinator.find1StageRocket(4500, 8).get
      
      rocket.stages.toList
      
      def printStageInfo(stages: List[Stage], isGroundStage: Boolean = true): Unit = stages match {
        case Nil =>
        case stage :: topStages =>
          println(s"-------------- Stage ${rocket.stages.size + stage.number} ---------------")
          stage.parts.foreach {
            case (part, count) => println(count +" x " + part.name)
          }
          println()
          println(s"Mass: " + stage.totalMass)
          val massOnTop = topStages.map(_.totalMass).sum
          if (isGroundStage) {
            println(s"dV ground: " + stage.deltaVsea(massOnTop))  
          } else {
            println(s"dV: " + stage.deltaVspace(massOnTop))
          }
          println(s"TWR: " + stage.thrustToWeight(massOnTop))
          println()
          printStageInfo(topStages, false)
      }
      
      
      /*rocket.stages.foreach { stage =>
        println(s"-------------- Stage ${rocket.stages.size + stage.number} ---------------")
        stage.parts.foreach {
          case (part, count) => println(count +" x " + part.name)
        }
        
        
        println()
      }*/
      
      printStageInfo(rocket.stages.toList, true)
      println()
      println()
      println("mass: " + rocket.mass)
      println("dV space: " + rocket.deltaV_space)
      println("dV from ground: " + rocket.deltaV_fromGround)
      println("TWR: " + rocket.firstStageTwr)
      
      val stopT = Platform.currentTime
      println()
      println(s"Time: ${stopT - startT} ms")
      success
    }
  }
  
  
  
}
