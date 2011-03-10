package dlwh.newcognates

import scalanlp.config.Configuration
import java.io.File
import scalanlp.stats.sampling.Rand
import scalanlp.fst.{DecayAutomaton, EditDistance}
import dlwh.cognates.{PosUniCompression, UniCompression, NormalizedTransitions, BiCompression}
;

/**
 * 
 * @author dlwh
 */
class TreeScorer[F<:Factors](tree: Tree, factors: F) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) = {
    val infer = new TreeInference(factors,tree,groupA);
    {(groupB: CognateGroup) =>
      val bigGroup = groupA merge groupB;
      val infer = new TreeInference(factors,tree,bigGroup);
      val ll = infer.onePassBeliefs.downward.likelihood;
//      val ll = infer.onePassBeliefs.likelihood;
      println(groupA + "X" + groupB + " " + ll);
      ll
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
    println(data.length);
    val randomized = Rand.permutation(data.length).draw().map(data);
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);

//    val factors = new BigramFactors;
    val compressor = readAutomataCompressor(config, "transducers.message");
    def editDistance(l: Language, l2: Language) = new EditDistance(-0.3,-0.4,alphabet);
    def initBelief(l: Language) = new DecayAutomaton(5, alphabet);
    def initMessage(a: Language, b: Language) = new DecayAutomaton(40, alphabet);
    val factors = new TransducerFactors(alphabet, compressor, initBelief(""), editDistance _,  initMessage _);

    val grouperFactory = BipartiteGrouper.factory(languages, -3)
    val scorerFactory = GlossRestrictedScorer.factory(TreeScorer.factory(tree, factors));
    val bipartite = new CognateDetector(grouperFactory, scorerFactory);
    val iter = bipartite.iterations(randomized);
    for( s <- iter.take(100)) { println(s.groups) }
  }

  case class MessageParams(`type`: String, klThreshold: Double, maxStates: Int);

  private def readAutomataCompressor(config: Configuration, prefix: String): MessageCompressor[_] = {
    val MessageParams(tpe, klThreshold, maxStates) = config.readIn[MessageParams](prefix);
    val beginningUnigram = '#';
    tpe match {
      case "bigram" | "bi" =>
        new BiCompression(klThreshold,maxStates,beginningUnigram) with NormalizedTransitions[Option[Char],Char] : MessageCompressor[_];
      case "unigram" | "uni" =>
        new UniCompression(beginningUnigram) with NormalizedTransitions[Unit,Char] : MessageCompressor[_];
      case "posunigram" | "posuni" =>
        new PosUniCompression(maxStates,beginningUnigram) with NormalizedTransitions[Int,Char] : MessageCompressor[_];
    }
  }
}