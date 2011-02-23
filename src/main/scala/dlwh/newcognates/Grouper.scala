package dlwh.newcognates

/**
 * 
 * @author dlwh
 */
trait Grouper {
  /**
   * Consider merging first group with second group, leaving third group untouched
   */
  def determineSplit(groups: IndexedSeq[CognateGroup]):(IndexedSeq[CognateGroup],IndexedSeq[CognateGroup],IndexedSeq[CognateGroup]);
  def determineGroupsToMerge(aff: AffinityScorer,
                             groupA: IndexedSeq[CognateGroup],
                             groupB: IndexedSeq[CognateGroup]):IndexedSeq[(CognateGroup,CognateGroup)]
}

object Grouper {
  trait Factory[G<:Grouper] {
    def initialGrouper: G
    def nextGrouper(curGrouper: G):G
  }
}


