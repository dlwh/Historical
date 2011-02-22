package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._
import scalanlp.optimize.KuhnMunkres
;

/**
 * 
 * @author dlwh
 */
class BipartiteGrouper(languages: Stream[Language], treePenalty: Double) extends Grouper {
  // GroupA is the held out language, groupB is the current groups.
  def determineGroupsToMerge(aff: AffinityScorer, groupA: IndexedSeq[CognateGroup], groupB: IndexedSeq[CognateGroup]) = {
    val affinities = groupB.par.map { b =>
      groupA.map{ a => -aff(a,b)};
    }

    val assignments = doMatching(affinities)
    val baselines = baselineScores(aff, groupA);
    val groupsToMerge = for {
      (w,g) <- assignments.iterator.zipWithIndex
      if w != -1 && w < groupA.length
      affScore = affinities(g)(w);
      if affScore <= baselines(w)
    } yield (groupA(w),groupB(w));

    groupsToMerge.toIndexedSeq;
  }

  final def doMatching(affinities: IndexedSeq[IndexedSeq[Double]]):IndexedSeq[Int] = {
    if(affinities.size == 0) IndexedSeq[Int]();
    else {
      val bm = KuhnMunkres;
      val size = affinities.size max affinities(0).size;
      val paddedAffinities = Array.tabulate(size,size) { (x,y) =>
        if (x < affinities.length && y < affinities(x).length) affinities(x)(y)
        else 0.0;
      };
      val changes = bm.extractMatching(paddedAffinities.map(_.toIndexedSeq).toIndexedSeq)._1;
      changes.take(affinities.length);
    }
  }
  // score to keep a cognate separate
  def baselineScores(aff: AffinityScorer, words: Seq[CognateGroup]) = {
    val arr = words.toArray.map(g => aff(CognateGroup.empty,g));
    for( i <- 0 until arr.length) {
      arr(i) += treePenalty;
    }
    arr
  }

  def determineSplit(groups: IndexedSeq[CognateGroup]) = {
    val heldOutLang = languages.head;
    val heldOutWords = for (grp <- groups if grp.cognates.contains(heldOutLang)) yield grp.cognates(heldOutLang);
    val removed = groups.view.map(_ - heldOutLang).filterNot(_.cognates.isEmpty).toIndexedSeq;
    (heldOutWords.map(CognateGroup(_)),removed,IndexedSeq.empty);
  }
}