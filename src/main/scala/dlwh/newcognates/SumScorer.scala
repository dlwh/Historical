package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class SumScorer[A<:AffinityScorer,B<:AffinityScorer](val a: A, val b: B) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) ={
    val ac = a.calibrate(groupA);
    val bc = b.calibrate(groupA);
    (groupB: CognateGroup) => { ac(groupB) + bc(groupB)};
  }
}

object SumScorer {

  def factory[A<:AffinityScorer, B<:AffinityScorer](a: AffinityScorer.Factory[A],
                                                    b: AffinityScorer.Factory[B]) = new Factory(a,b);

  class Factory[A<:AffinityScorer, B<:AffinityScorer](a: AffinityScorer.Factory[A],
                                                      b: AffinityScorer.Factory[B]) extends AffinityScorer.Factory[SumScorer[A,B]] {
    def initialScorer: SumScorer[A, B] = new SumScorer(a.initialScorer,b.initialScorer);

    def nextScorer(curScorer: SumScorer[A, B], groups: IndexedSeq[CognateGroup]) = {
      new SumScorer(a.nextScorer(curScorer.a,groups),b.nextScorer(curScorer.b, groups));
    }
  }
}