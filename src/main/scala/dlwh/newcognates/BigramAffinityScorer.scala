package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class BigramAffinityScorer extends AffinityScorer {
  def calibrate(a: CognateGroup)= { (b: CognateGroup) =>
    val scores = for {
      aa <- a.cognates.valuesIterator;
      aBag = bigrams(aa.word);
      bb <- b.cognates.valuesIterator
      bBag = bigrams(bb.word)
    } yield {
      (aBag intersect bBag size) * 1.0 / (bBag ++ aBag size);
    }

    (scores ++ Iterator(0.0)).max;
  }

  def bigrams(a: String) = {
    Set() ++ ("@" + a + "@").sliding(2);
  }
}


object BigramAffinityScorer {
  def factory:AffinityScorer.Factory[BigramAffinityScorer] = new AffinityScorer.Factory[BigramAffinityScorer] {
    def nextScorer(curScorer: BigramAffinityScorer, groups: IndexedSeq[CognateGroup]) = new BigramAffinityScorer

    def initialScorer = new BigramAffinityScorer;
  }
}