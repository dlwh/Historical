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

class GlossRestrictedScorer[Aff<:AffinityScorer](val innerScorer: Aff) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) = {
    val glosses = groupA.glosses;
    if(glosses.size > 1) { (groupB: CognateGroup) => -1E8}
    else {
      val inner = innerScorer.calibrate(groupA);
      {(groupB: CognateGroup) =>
        if(groupA.merge(groupB).glosses.size > 1) -1e8
        else inner(groupB);
      }
    }

  }
}

object GlossRestrictedScorer {
  def factory[Aff<:AffinityScorer](factory: AffinityScorer.Factory[Aff]) :AffinityScorer.Factory[GlossRestrictedScorer[Aff]] ={
    new AffinityScorer.Factory[GlossRestrictedScorer[Aff]] {
      def nextScorer(curScorer: GlossRestrictedScorer[Aff],
                     groups: IndexedSeq[CognateGroup]) = {
        val next = factory.nextScorer(curScorer.innerScorer,groups)
        new GlossRestrictedScorer(next);
      }

      def initialScorer = new GlossRestrictedScorer(factory.initialScorer)
    }
  }
}
