package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._
import scalanlp.optimize.KuhnMunkres
import java.io.File
import scalanlp.config.Configuration
import scalanlp.stats.sampling.Rand
import scalanlp.util.HeapDump
;

/**
 * 
 * @author dlwh
 */
class BipartiteGrouper(val languages: Stream[Language], val treePenalty: Double) extends Grouper {
  // GroupA is the held out language, groupB is the current groups.
  def determineGroupsToMerge(aff: AffinityScorer, groupA: IndexedSeq[CognateGroup], groupB: IndexedSeq[CognateGroup]) = {
    val affinities = groupB.map { b =>
      val cal = aff.calibrate(b);
      groupA.map{ a => -cal(a)};
    }

    val assignments = doMatching(affinities)
    val baselines = baselineScores(aff, groupA);
    val groupsToMerge = for {
      (w,g) <- assignments.iterator.zipWithIndex
      if w != -1 && w < groupA.length
    } yield {
      val affScore = affinities(g)(w);
      println(groupA(w) + " " + groupB(g) + " " + affScore + " " + baselines(w));
      if (affScore <= baselines(w)) (groupA(w),groupB(g))
      else (groupA(w),CognateGroup.empty);
    };

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
    val arr = words.toArray.map(g => -aff(CognateGroup.empty,g));
    for( i <- 0 until arr.length) {
      arr(i) -= treePenalty;
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

object BipartiteGrouper {
  def factory(languages: Seq[Language], treePenalty: Double): Grouper.Factory[BipartiteGrouper] = new Grouper.Factory[BipartiteGrouper] {


    def nextGrouper(curGrouper: BipartiteGrouper) =  { new BipartiteGrouper(curGrouper.languages.drop(1), curGrouper.treePenalty) }

    def initialGrouper = new BipartiteGrouper(Stream.continually(languages).flatten, treePenalty);
  }
}

object RunBigrams {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    println(withGloss);

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val gold = dataset.cognates.map(_.filter(cog => leaves(cog.language)));

    val data = gold.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);

    val bipartite = new CognateDetector(BipartiteGrouper.factory(languages,1), BigramAffinityScorer.factory)
    val iter = bipartite.iterations(randomized);
    for( s <- iter.take(100)) { println(s.groups) }
  }
}