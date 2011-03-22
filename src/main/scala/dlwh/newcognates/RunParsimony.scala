package dlwh.newcognates

import scalanlp.config.Configuration
import java.io.File
import scalanlp.util.Index
import scalanlp.fst.Alphabet
import scalanlp.fst.fast.{AutomatonFactory}
import collection.mutable.ArrayBuffer

class ParsimonyRunner(dataset: Dataset, legalGloss: Set[Symbol]) {
  val tree = dataset.tree
  val gold = dataset.cognates.filter(group => legalGloss(group.head.gloss));
  val alphabet = dataset.alphabet;

  val data = gold.flatten
  println(data.length);
  val legalWords = data.toSet;

  val editDistance = new ThreeStateEditDistance(alphabet) with FeaturizedOptimization;
  val factorFactory = new ParsimonyFactorsFactory(editDistance);
  import factorFactory.editDistance._;
  import factorFactory.EdgeExpectedCounts;
  val cognatesByGloss = legalWords.groupBy(_.gloss);
  val legalByGloss: Map[Symbol, Set[String]] = legalWords.groupBy(_.gloss).mapValues( cognates => cognates.map(_.word));
  val goldTags = Accuracy.assignGoldTags(gold);
  val allEdges = tree.edges;


  type ECounts = Map[Language,EdgeExpectedCounts];

  def sumCounts(s1: ECounts, s2: ECounts) = {
    val r = collection.mutable.Map[Language,EdgeExpectedCounts]();
    r ++= s1;
    for( (k,v) <- s2) {
      if(r.contains(k)) r(k) += v
      else r(k) = v;
    }
    r.toMap
  }

  case class State(cognates: IndexedSeq[CognateGroup], parameters: Map[Language,Parameters])

  private def initialState = State(IndexedSeq.empty,
    Map.empty[Language,Parameters].withDefaultValue(initialParameters))


  def iterator:Iterator[State] = { Iterator.iterate(initialState) { case State(cogs,costs) =>
    def innovationProb(a: Language, b: Language) = if(cogs.isEmpty) 1E-1 else 1e-4;
    val groupsAndCounts = (for( (gloss,cognates) <- cognatesByGloss) yield {
      val factors = new factorFactory.WordFactors(cognates.map(_.word).toSet, costs, innovationProb _, viterbi = true);
      val inference: ParsimonyInference[factorFactory.WordFactors] = new ParsimonyInference(factors,tree,cognatesByGloss(gloss).toSeq);
      printMarginals(gloss, inference)
      printGold(gloss)

      val myGroups = decodeCognates(inference,tree).filterNot(_.isEmpty);
//      val myGroups = iterativeDecode(cognates.toIndexedSeq,costs,tree, if(cogs.isEmpty) 1E-1 else 1e-4);
      println(myGroups);
      assert(myGroups.iterator.map(_.size).sum == cognatesByGloss(gloss).size);


      val flattenedGroups: IndexedSeq[CognateGroup] = myGroups.map(a => CognateGroup(a:_*));
      // expectedCounts
      val edgeCounts = (for (pair@(parent, child) <- allEdges.iterator;
                             marginal <- inference.edgeMarginal(parent, child).iterator;
                             edge <- marginal.iterator)
      yield {pair._2 -> edge.expectedCounts}).toMap;

      (flattenedGroups,edgeCounts);
    }).toIndexedSeq;


    val guessGroups: IndexedSeq[CognateGroup] = groupsAndCounts.iterator.map(_._1).reduceLeft(_ ++_);
    val counts = groupsAndCounts.iterator.map(_._2).reduceLeft(sumCounts);
    val newCosts = factorFactory.editDistance.makeParameters(counts.mapValues(_.alignmentCounts));
    State(guessGroups,newCosts);
  }}.drop(1)


  def printMarginals(gloss: Symbol, inference: ParsimonyInference[factorFactory.WordFactors]): Unit = {
    println(gloss + " " + inference.likelihood);
    val string = tree.prettyString {
      lang =>
        if (!inference.hasUpwardMessage(lang)) None
        else {
          if (lang == tree.label) Some(lang + ": " + inference.beliefs(lang));
          else Some(lang + ": " + inference.beliefs(lang) + " " + inference.edgeMarginal(inference.parent(lang), lang).get.map(_.posteriorInnovationProb));
        }
    }
    println(string);
  }
  def printGold(gloss: Symbol): Unit = {
    val byLanguage = cognatesByGloss(gloss).groupBy(_.language).mapValues(_.toIndexedSeq);
    val goldTree = GoldStatus.buildTree(tree, byLanguage, goldTags).get._1;
    println(goldTree.prettyString(Some(_)));
  }

  def iterativeDecode(cognates: Seq[Cognate], params: Map[Language,factorFactory.editDistance.Parameters], tree: Tree, innovProb:Double) = {
    val groups = new ArrayBuffer[IndexedSeq[Cognate]];

    def innovationProb(a: String, b:String) =  innovProb;
    val factors = new factorFactory.WordFactors(cognates.map(_.word).toSet, params, innovationProb _, viterbi = true);
    val inference = new ParsimonyInference(factors,tree,cognates)
    val group = decodeCognates(inference,tree).filterNot(_.isEmpty).head;
    println("initial: " + group);
    groups += group;

    var remainingWords = cognates.toSet -- group;
    while(!remainingWords.isEmpty) {
      val theWord = remainingWords.head;
      println("considering " + theWord + " from " + remainingWords);
      val wordsToConsider = remainingWords.filterNot(_.language == theWord.language) + theWord toIndexedSeq

      val pathToRoot = CognateGroup(theWord).nodesWithObservedDescendants(tree);
      def innovationProb(a: String, b:String) = if(pathToRoot(b)) 0.0 else 1E-4;

      val factors = new factorFactory.WordFactors(cognates.map(_.word).toSet, params, innovationProb _, viterbi = true);
      val inference = new ParsimonyInference(factors,tree,wordsToConsider)
      printMarginals(theWord.gloss, inference)

      val group = decodeCognates(inference,tree).filterNot(_.isEmpty).head;
      assert(group.contains(theWord),group);
      println("got " + group);
      groups += group;
      remainingWords --= group;
    }
    groups;
  }

  def decodeCognates(inference: ParsimonyInference[factorFactory.WordFactors],
                     tree: Tree,
                     cur: ArrayBuffer[Cognate]= new ArrayBuffer[Cognate]):IndexedSeq[IndexedSeq[Cognate]] = tree match {
    case t: Child => error("no")
    case Ancestor(label,children) =>
      val childGroups = for(c <- children if inference.hasUpwardMessage(c.label)) yield {
        val edges = inference.edgeMarginal(label,c.label).get;
        val fromChild = if(c.isInstanceOf[Child]) {
          var rest = IndexedSeq.empty[Cognate];
          for( (w,e) <- inference.groupedByLanguage(c.label) zip edges) {
            if(e.posteriorInnovationProb >= 0.5) rest :+= w
            else cur += w
          }
          rest.map(IndexedSeq(_));
        } else if(edges.head.posteriorInnovationProb < 0.5){
          decodeCognates(inference,c,cur).drop(1)
        } else {
          decodeCognates(inference,c);
        }
        fromChild;
      }

      (cur +: (childGroups.flatten.filterNot(_.isEmpty))).toIndexedSeq;
  }

}



/**
 *
 * @author dlwh
 */

object RunParsimony {
  def main(args: Array[String]) {
    val legalGloss = args(0).split(':').map(Symbol(_)).toSet;
    println(legalGloss);
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val dataset = Dataset.fromConfiguration(config);

    val learningEpochs = config.readIn[Int]("learningEpochs",10);

    val runner = new ParsimonyRunner(dataset,legalGloss);
    import runner.gold;
    import runner.goldTags;

    println(gold.length);
    val numGold = Accuracy.numGold(gold);

    for((runner.State(guessGroups,costs),iter) <- runner.iterator.take(learningEpochs).zipWithIndex) {
      val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, guessGroups)
      val f1 = 2 * (precision * recall) / (precision + recall)
      println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + f1 + "\t" + guessGroups.length + "\t" + Accuracy.purity(goldTags,guessGroups));
    }

  }



}

  /*
def decodeCognates(inference: ParsimonyInference[factorFactory.WordFactors], tree: Tree):IndexedSeq[Cognate] = tree match {
case t: Child => inference.groupedByLanguage(t.label).toIndexedSeq;
case Ancestor(label, children) =>
val childGroups = for(c <- children if inference.hasUpwardMessage(c.label)) yield {
val edges = inference.edgeMarginal(label,c.label).get;
val fromChild = if(c.isInstanceOf[Child]) {
for( (w,e) <- inference.groupedByLanguage(c.label) zip edges if e.posteriorInnovationProb > .5) yield {
w
}
} else if(edges.head.posteriorInnovationProb > 0.5) {
decodeCognates(inference,c)
} else {
IndexedSeq.empty
}
fromChild.toIndexedSeq
}
childGroups.flatten.toIndexedSeq;
}

*/