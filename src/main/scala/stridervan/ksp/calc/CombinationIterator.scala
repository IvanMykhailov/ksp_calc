package stridervan.ksp.calc

import scala.collection.mutable


/**
 * Generate all possible combinations of elements, each element can present multiple times.
 * Total element count in each combination is elementCountLimit
 * 
 * Produce Seq[Int] with size "elements" where seq(i) = count of element i in this combination 
 */
class CombinationIterator(elements: Int, elementCountLimit: Int) extends Iterator[Seq[Int]] {

  var combination: Option[Seq[Int]] = Some(Seq.fill(elements - 1)(0) :+ elementCountLimit)
  
        
  def hasNext = combination.isDefined
  def next() = {
    val current = combination.get
    combination = nextCombination(current, elementCountLimit)
    current
  }
  
  
  def nextCombination(state: Seq[Int], limit: Int): Option[Seq[Int]] = {
    if (state.size < 2) throw new IllegalArgumentException("Just 2 or less parts? It will not fly! " + state)
    if (state.sum != limit) throw new IllegalArgumentException("state.sum != limit " + state)
    
    var modifiableState = mutable.Seq(state.init :_*)
    val lastElem = state.last
    modifiableState(0) = modifiableState(0) + 1
    
    var overflow = false 
    while (modifiableState.sum > limit && !overflow) {
      val i = modifiableState.indexWhere(_>0)
      if (i == modifiableState.size-1) {
        overflow = true
      } else {
        modifiableState(i) = 0
        modifiableState(i+1) = modifiableState(i+1) + 1
      }
    }
    
    if (overflow) {
      None
    } else {
      val rez = Seq(modifiableState :+ (limit - modifiableState.sum):_*)
      Some(rez)
    }
  }
}


class CombinationIteratorM(elements: Int, elementCountLimit: Int) extends Iterator[Seq[Int]] {

  var overflow = false
  
  var combination: mutable.Seq[Int] = mutable.Seq.fill(elements - 1)(0) :+ elementCountLimit
  
        
  def hasNext = !overflow
  def next() = {
    val current = Seq(combination:_*)
    nextCombination()
    current
  }
  
  
  def nextCombination(): Unit = {
    if (combination.size < 2) throw new IllegalArgumentException("Just 2 or less parts? It will not fly! " + combination)
    if (combination.sum != elementCountLimit) throw new IllegalArgumentException("state.sum != limit " + combination)
    
    val lastElem = combination.last
    combination(0) = combination(0) + 1
    combination(combination.size - 1) = 0
    
    while (combination.sum > elementCountLimit && !overflow) {
      val i = combination.indexWhere(_>0)
      if (i == combination.size-2) {
        overflow = true
      } else {
        combination(i) = 0
        combination(i+1) = combination(i+1) + 1
      }
    }    
    combination(combination.size - 1) = elementCountLimit - combination.sum
  }
}