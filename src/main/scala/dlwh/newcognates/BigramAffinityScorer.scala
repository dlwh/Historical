package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class BigramAffinityScorer extends AffinityScorer {
  def determineAffinities(a: CognateGroup, b: CognateGroup) = {
    val scores = for {
      aa <- a.cognates.valuesIterator;
      aBag = bigrams(aa.word);
      bb <- b.cognates.valuesIterator
      bBag = bigrams(bb.word)
    } yield {
      (aBag intersect bBag size) * 1.0 / (bBag ++ aBag size);
    }

    scores.max
  }

  def bigrams(a: String) = {
    Set() ++ ("@" + a + "@").sliding(2);
  }
}
