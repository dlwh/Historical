package dlwh.cognates

/**
 * Evaluates the pairwise precision, pairwise recall and cluster purity of a clustering
 * @author dlwh
 */
class CognatesEval(val goldClusters: Seq[Seq[Cognate]]) {
  val gold =  (for((group,index) <- goldClusters.zipWithIndex; c <- group) yield (c,index)) toMap

  private val numGold = {
    goldClusters.foldLeft(0)( (acc,set) => acc + (set.size) * (set.size- 1) / 2);
  }

  def precisionAndRecall(cognates: Seq[CognateGroup]) = {
    var numCorrect = 0;
    var numGuesses = 0;
    for(group <- cognates) {
      val gs = group.cognates.toIndexedSeq;
      for(i <- 0 until gs.length; j <- (i+1) until gs.length) {
        if(gold(gs(i)) == gold(gs(j)))
          numCorrect += 1
        numGuesses += 1
      }
    }

    val r= (numCorrect * 1.0 / numGuesses,numCorrect * 1.0 / numGold);
    println(r,numGuesses,numGold);
    r
  }

  def purity(cognates: Seq[CognateGroup]) = {
    val numberAssignedCorrectly = cognates.iterator.map { group =>
      val goldClusters: Iterator[Int] = group.cognates.iterator.map(c => gold(c));
      scalala.library.Library.count(goldClusters).max;
    } reduceLeft(_+_);
    val numGroups = gold.size;
    numberAssignedCorrectly * 1.0 / numGroups;
  }
}