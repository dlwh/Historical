package dlwh.newcognates

import scalanlp.config.Configuration
import java.io.File
import scalanlp.stats.sampling.Rand;

/**
 * 
 * @author dlwh
 */
class TreeScorer[F<:Factors](tree: Tree, factors: F) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) = {
    val infer = new TreeInference(factors,tree,groupA);
    lazy val beliefs = infer.onePassBeliefs;
    {(groupB: CognateGroup) =>
      if(groupB.cognates.size == 1) {
        val (language,cog) = groupB.cognates.iterator.next;
        beliefs.belief(language)(cog.word);
      } else if(groupB.cognates.isEmpty) {
        beliefs.likelihood;
      } else {
        val bigGroup = groupA merge groupB;
        val infer = new TreeInference(factors,tree,bigGroup);
        infer.onePassBeliefs.likelihood;
      }
    }
  }
}

object TreeScorer {
  def factory[F<:Factors](tree: Tree, f: F):AffinityScorer.Factory[TreeScorer[F]] = new AffinityScorer.Factory[TreeScorer[F]] {
    def nextScorer(curScorer: TreeScorer[F], groups: IndexedSeq[CognateGroup]) = new TreeScorer(tree,f);

    def initialScorer = new TreeScorer[F](tree,f);
  }
}

object RunTreeBigrams {
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

    val factors = new BigramFactors;

    val bipartite = new CognateDetector(BipartiteGrouper.factory(languages,1), TreeScorer.factory(tree, factors));
    val iter = bipartite.iterations(randomized);
    for( s <- iter.take(100)) { println(s.groups) }
  }
}