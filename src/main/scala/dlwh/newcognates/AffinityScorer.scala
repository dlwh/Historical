package dlwh.newcognates

/**
 * 
 * @author dlwh
 */
trait AffinityScorer extends ((CognateGroup,CognateGroup)=>Double) {
  def determineAffinities(groupA: CognateGroup, groupB: CognateGroup):Double = calibrate(groupA)(groupB);
  def apply(groupA: CognateGroup, groupB: CognateGroup):Double = determineAffinities(groupA,groupB);
  def calibrate(groupA: CognateGroup):(CognateGroup=>Double)
}

object AffinityScorer {
  trait Factory[A<:AffinityScorer] {
    def initialScorer: A
    def nextScorer(curScorer: A, groups: IndexedSeq[CognateGroup]):A
  }
}

