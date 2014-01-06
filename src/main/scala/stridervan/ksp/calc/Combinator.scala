package stridervan.ksp.calc

object Combinator {

  def partCombinations(parts: Seq[RocketPart], maxPartCount: Int): Iterable[Map[RocketPart, Int]] = {
    new CombinationIteratorM(parts.size, maxPartCount)
    .toIterable.map { combination => 
    
      var map = Map.empty[RocketPart, Int]
      for (i <- 0 to (combination.size -1) if combination(i) > 0) {
        map += parts(i) -> combination(i)
      }
      map
    }    
  }  
    
  
  def find1StageRocket(reqiredDeltaV: Double, workloadMass: Double): Option[Rocket] = {
    val workload = Load("Workload", workloadMass)
    val topStage = Stage(0, Map(workload -> 1)) 
    
    var bestRocket: Option[Rocket] = None
    
    
    def addStages(stagesLeft: Int, topStages: Seq[Stage], stageDeltaV: Double): Unit = {
      if (stagesLeft == 0) {
        val rocket = Rocket(topStages)
        if (FitnessFunction.toKerbalOrbit(rocket)) {
          bestRocket match {
            case None => bestRocket = Some(rocket) 
            case Some(best) => 
              if (rocket.mass < best.mass) bestRocket = Some(rocket) 
          }
        }
      } else {
        val topStagesMass = topStages.map(_.totalMass).sum
        
        for {
          numberOfEngines <- 1 to 4
          engines <- partCombinations(parts.engines, numberOfEngines).par
          numberOfTanks <- 1 to (64*2)
          //tanks <- partCombinations(parts.tanks, numberOfTanks)
        } {
      
          val tanks = Map(parts.byName("FL-T100") -> numberOfTanks)
          val stageParts = engines ++ tanks + (parts.separator -> 1)
          val stage = Stage(topStages.head.number - 1, stageParts)
          lazy val stageDeltaV = stage.deltaVspace(topStagesMass)          
          val isGroundStage = (stagesLeft == 1)
          
          if (
            (
              (!isGroundStage) && 
              (stageDeltaV > stageDeltaV * 0.5) &&
              (stageDeltaV < stageDeltaV * 1.5) &&
              (stage.thrustToWeight(topStagesMass) > 0.5)
              || 
              (isGroundStage) &&
              (stage.thrustToWeight(topStagesMass) > 1)
            )
            && (stage.totalMass >= topStagesMass)
            && (stage.totalMass + topStagesMass < workloadMass * 10)
            
          ) {
            addStages(stagesLeft - 1, Seq(stage) ++ topStages, stageDeltaV)
          }
          
        }
      }
    }
    
    for (stageCount <- 1 to 2) {
      val stageDeltaV = (reqiredDeltaV / stageCount)
      addStages(stageCount, Seq(topStage), stageDeltaV)
    }
    
    
    /*
    for {
      numberOfEngines <- 1 to 5
      engines <- partCombinations(parts.engines, numberOfEngines).par
      numberOfTanks <- 1 to 5
      tanks <- partCombinations(parts.tanks, numberOfTanks)
    } {
      
      val stageParts = engines ++ tanks + (parts.separator -> 1)
      val stage1 = Stage(1, stageParts)
      val rocket = Rocket(Seq(stage1, topStage))
      
      if (FitnessFunction.toKerbalOrbit(rocket)) {
        bestRocket match {
          case None => bestRocket = Some(rocket) 
          case Some(best) => 
            if (rocket.mass < best.mass) bestRocket = Some(rocket) 
        }
      }
    }
    */
    
    bestRocket
  }
  
}