package dlwh.newcognates

import scalanlp.config.Configuration
import java.io.File
import scalanlp.concurrent.ParallelOps._;
import collection.mutable.ArrayBuffer

class ParsimonyRunner(dataset: Dataset, legalGloss: Set[Symbol], innovationProb: Double, forkThreshold: Double = 0.99) {
  val tree = dataset.tree
  val gold = dataset.cognates.filter(group => legalGloss(group.head.gloss));
  val alphabet = dataset.alphabet;

  val data = gold.flatten
  println(data.length);
  val legalWords = data.toSet;


//  val editDistance = new ThreeStateEditDistance(alphabet, -3, -3) with FeaturizedOptimization;
  val initSubs = Array(-3,-5,-2,-4,-5,-6,-5,1)
  val initIns = Array(-3,1,-4,-3,-2,-1,-4,-1)
  def subRatio(state: Int) = initSubs(state)
  def insRatio(state: Int) = initIns(state)
  def transRatio(from: Int, to:Int) = if(from == to) 0.0 else -3.0;
  val editDistance = new GeneralEditDistance(4,alphabet, subRatio, insRatio, transRatio) {
    /*
    override def initialParameters = new Parameters {
      def apply(s: Int, t: Int, c1: Int, c2: Int) = {
        if(s > t) Double.NegativeInfinity
        else if( t ==0 || t == 2) if(c1 != pe.epsIndex) Double.NegativeInfinity else -3
        else if (t == 1) if(c1 == c2) 0.0 else -4
        else Double.NegativeInfinity;
      }

      def finalCost(s: Int) = if(s == 0) Double.NegativeInfinity else 0.0

      def initialStateWeight(s: Int) = if(s == 0.0) 0.0 else Double.NegativeInfinity
    }
    */

  }
  val wordFactory: WordFactorsFactory = new WordFactorsFactory(editDistance);
//  val wordFactory2: WordFactorsFactory = new WordFactorsFactory(new ThreeStateEditDistance(alphabet,-2,-2) with FeaturizedOptimization);
  val innovFactory = new LanguageModelFactorsFactory(alphabet);
  val factorFactory = new ParsimonyFactorsFactory(wordFactory, innovFactory, 0.5, 0.3);
  import factorFactory._;
  val cognatesByGloss = legalWords.groupBy(_.gloss);
  val legalByGloss: Map[Symbol, Set[String]] = legalWords.groupBy(_.gloss).mapValues( cognates => cognates.map(_.word));
  val goldTags = Accuracy.assignGoldTags(gold);
  val allEdges = tree.edges;


  type ECounts = Map[Language,SufficientStatistics];

  def sumCounts(s1: ECounts, s2: ECounts) = {
    val r = collection.mutable.Map[Language,SufficientStatistics]();
    r ++= s1;
    for( (k,v) <- s2) {
      if(r.contains(k)) r(k) += v
      else r(k) = v;
    }
    r.toMap
  }

  case class State(cognates: IndexedSeq[CognateGroup], parameters: Map[Language,EdgeParameters], likelihood: Double, iter: Int)

  private def initialState = State(IndexedSeq.empty,
    Map.empty[Language,EdgeParameters].withDefaultValue(initialParameters), Double.NegativeInfinity, 0)


  def iterator:Iterator[State] = { Iterator.iterate(initialState) { case State(cogs,costs, oldLL, iter) =>
    def mapStep(gloss: Symbol, cognates: Set[Cognate]) = {
      val factors = factorFactory.mkFactors(cognates.map(_.word).toSet, costs);
      val inference: ParsimonyInference[factorFactory.Factors] = new ParsimonyInference(factors,tree,cognatesByGloss(gloss).toSeq);
      printMarginals(gloss, inference)
      printGold(gloss)

      val myGroups = decodeCognates(inference,tree).filterNot(_.isEmpty);
//       val myGroups = newDecode(inference,tree).filterNot(_.isEmpty);
//      val myGroups = iterativeDecode(cognates.toIndexedSeq,costs,tree, if(cogs.isEmpty) 1E-1 else 1e-4);
      println(myGroups);
      assert(myGroups.iterator.map(_.size).sum == cognatesByGloss(gloss).size);


      val flattenedGroups: IndexedSeq[CognateGroup] = myGroups.map(a => CognateGroup(a:_*));
      // expectedCounts
      val edgeCounts = (for (pair@(parent, child) <- allEdges.iterator;
                             marginal <- inference.edgeMarginal(parent, child).iterator;
                             edge <- marginal.iterator)
      yield {pair._2 -> edge.sufficientStatistics}).toMap;

      (flattenedGroups,edgeCounts, inference.likelihood);
    }

    def sumGroups(g1: IndexedSeq[CognateGroup], g2: IndexedSeq[CognateGroup]) = g1 ++ g2;
    def reduceStep( g1: (IndexedSeq[CognateGroup], ECounts, Double), g2: (IndexedSeq[CognateGroup],ECounts, Double)) = (sumGroups(g1._1,g2._1),sumCounts(g1._2,g2._2),g1._3 + g2._3);

    val (guessGroups,counts, ll) = cognatesByGloss.toIndexedSeq.par(4).mapReduce(Function.tupled(mapStep), reduceStep _);
    println("ll" + ll);
    println("preopt");
    var newCosts =  factorFactory.optimize(counts);
//    if(iter > 4) newCosts = newCosts.mapValues(_.enableHomoplasy);
    println(guessGroups,newCosts);
    State(guessGroups,newCosts, ll, iter + 1);
  }}.drop(1)


  def printMarginals(gloss: Symbol, inference: ParsimonyInference[factorFactory.Factors]): Unit = {
    println(gloss + " " + inference.likelihood);
    val string = tree.prettyString {
      lang =>
        if (!inference.hasUpwardMessage(lang)) None
        else {
          if (lang == tree.label) Some(lang + ": " + inference.beliefs(lang));
          else Some(lang + ": " + inference.beliefs(lang) + " " + inference.edgeMarginal(inference.parent(lang), lang));
        }
    }
    println(string);
  }
  def printGold(gloss: Symbol): Unit = {
    val byLanguage = cognatesByGloss(gloss).groupBy(_.language).mapValues(_.toIndexedSeq);
    val goldTree = GoldStatus.buildTree(tree, byLanguage, goldTags).get._1;
    println(goldTree.prettyString(Some(_)));
  }

  /*

  def newDecode(inference: ParsimonyInference[factorFactory.Factors],
                tree: Tree,
                cur: ArrayBuffer[Cognate] = new ArrayBuffer[Cognate]):IndexedSeq[IndexedSeq[Cognate]] = tree match {
    case t: Child => error("no")
    case Ancestor(label,children) =>
      val lib = new ArrayBuffer[Cognate];
      val childGroups = for(c <- children if inference.hasUpwardMessage(c.label)) yield {
        val edges = inference.edgeMarginal(label,c.label).get;
        val fromChild = if(c.isInstanceOf[Child]) {
          val rest = new ArrayBuffer[Cognate];
          for( (w,e:inference.factors.Edge) <- inference.groupedByLanguage(c.label) zip edges) {
            val target = {
              if(e.posteriorInnovationProb > (e.posteriorFromInnovativeParent max e.posteriorConservativeProb).min(forkThreshold)) rest
              else if(e.posteriorFromInnovativeParent > e.posteriorConservativeProb) lib;
              else cur
            }
            target += w;
          }
          rest.map(IndexedSeq(_));
        } else {
          val e: inference.factors.Edge = edges.head;
          val target = {
            if(e.posteriorInnovationProb > (e.posteriorFromInnovativeParent max e.posteriorConservativeProb).min(forkThreshold)) new ArrayBuffer[Cognate]();
            else if(e.posteriorFromInnovativeParent > e.posteriorConservativeProb) lib;
            else cur
          }
          val res = newDecode(inference,c,target);
          if(e.posteriorInnovationProb > (e.posteriorFromInnovativeParent max e.posteriorConservativeProb).min(forkThreshold)) res
          else res.drop(1);
        }
        fromChild;
      }

      (cur +: lib +: (childGroups.flatten.filterNot(_.isEmpty))).toIndexedSeq;
  }

  def iterativeDecode(cognates: Seq[Cognate], params: Map[Language,EdgeParameters], tree: Tree, innovProb:Double) = {
    val groups = new ArrayBuffer[IndexedSeq[Cognate]];

    def innovationProb(a: String, b:String) =  innovProb;
    val factors = factorFactory.mkFactors(cognates.map(_.word).toSet, params);
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
      val adjParams = params.map { case (l,param) =>
        l -> {if(pathToRoot(l)) param.copy(innovationProb = 0.0) else param};
      };

      val factors = factorFactory.mkFactors(cognates.map(_.word).toSet, adjParams);
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
  */

  def decodeCognates(inference: ParsimonyInference[factorFactory.Factors],
                     tree: Tree,
                     cur: ArrayBuffer[Cognate]= new ArrayBuffer[Cognate]):IndexedSeq[IndexedSeq[Cognate]] = tree match {
    case t: Child => error("no")
    case Ancestor(label,children) =>
      val childGroups = for(c <- children if inference.hasUpwardMessage(c.label)) yield {
        val edges = inference.edgeMarginal(label,c.label).get;
        val fromChild = if(c.isInstanceOf[Child]) {
          var rest = IndexedSeq.empty[Cognate];
          for( (w,e:inference.factors.Edge) <- inference.groupedByLanguage(c.label) zip edges) {
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
    val innovationProb = config.readIn[Double]("innovationProb",0.996);

    val learningEpochs = config.readIn[Int]("learningEpochs",15);

    val runner = new ParsimonyRunner(dataset,legalGloss, innovationProb);
    import runner.gold;
    import runner.goldTags;

    println(gold.length);
    val numGold = Accuracy.numGold(gold);
    val evalLanguage = config.readIn[String]("evalLanguage");

    val goldAssignments = assignWordsToGold(dataset.base.cognates, evalLanguage)

    new File("outcognates").mkdirs();
    for(runner.State(guessGroups,costs,likelihood,iter) <- runner.iterator.take(learningEpochs)) {
      val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, guessGroups)
      val f1 = 2 * (precision * recall) / (precision + recall)
      println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + f1 + "\t" + guessGroups.length + "\t" + likelihood + "\t" + Accuracy.purity(goldTags,guessGroups));
      CognateGroup.writeCognateGroups(guessGroups,dataset.languages,new File("outcognates/cognates" + iter), evalLanguage, goldAssignments);
    }



  }

  def assignWordsToGold(gold: Seq[Seq[Cognate]], language: Language) = {
    val pairs  = for(group <- gold iterator;
        goldWord <- group.find(_.language == language) iterator;
        c <- group iterator) yield c -> goldWord;

    pairs.toMap;
  }



}

object RunParsimonyIntoAgglomerative {
  def main(args: Array[String]) {
    val legalGloss = args(0).split(':').map(Symbol(_)).toSet;
    println(legalGloss);
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val dataset = Dataset.fromConfiguration(config);
    val innovationProb = config.readIn[Double]("innovationProb",0.996);

    val learningEpochs = config.readIn[Int]("learningEpochs",10);
    val startFromState = config.readIn[Boolean]("startFromParsimony", false)

    val runner = new ParsimonyRunner(dataset,legalGloss, innovationProb);
    import runner.gold;
    import runner.goldTags;

    println(gold.length);
    val numGold = Accuracy.numGold(gold);

    val evalLanguage = config.readIn[String]("evalLanguage");

    val goldAssignments = assignWordsToGold(dataset.base.cognates, evalLanguage)

    new File("outcognates").mkdirs()
    var lastState : runner.State = null;
    for( state @ runner.State(guessGroups,costs,likelihood,iter) <- runner.iterator.take(learningEpochs)) {
      lastState = state;
      val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, guessGroups)
      val f1 = 2 * (precision * recall) / (precision + recall)
      println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + f1 + "\t" + guessGroups.length + "\t" + likelihood + "\t" + Accuracy.purity(goldTags,guessGroups));
      CognateGroup.writeCognateGroups(guessGroups,dataset.languages,new File("outcognates/cognates" + iter), evalLanguage, goldAssignments);
    }

    val bipartite = new NewAgglomerative(runner.tree, 20) {
      val factory : runner.factorFactory.type = runner.factorFactory;
    };

    val factors = runner.cognatesByGloss.mapValues { words =>
      bipartite.factory.mkFactors(words.map(_.word), lastState.parameters);
    }

    val iter = bipartite.iterations( if(startFromState) lastState.cognates else runner.data.map(CognateGroup(_)), factors);

    var lastLL = Double.NegativeInfinity;
    var qqqed = false
    for( (s,iter) <- iter.zipWithIndex) {
      println("start agglom iter!")
        val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, s.groups)
        val f1 = 2 * (precision * recall) / (precision + recall)
        println("::: " + iter + "\t"+ precision + "\t" + recall +"\t" + s.likelihood  + "\t" + f1 + "\t" + s.groups.length + "\t" + Accuracy.purity(goldTags,s.groups));
        if(s.groups.length == gold.length) println(":: ===============================================");
        else if(lastLL > s.likelihood && !qqqed) { qqqed = true; println(":: QQQQ") }
        lastLL = s.likelihood;
        if(iter % 20 ==0)
          CognateGroup.writeCognateGroups(s.groups,dataset.languages,new File("outcognates/agglom" + iter), evalLanguage, goldAssignments);
        println(s.groups)
      }

  }

  def assignWordsToGold(gold: Seq[Seq[Cognate]], language: Language) = {
    val pairs  = for(group <- gold iterator;
        goldWord <- group.find(_.language == language) iterator;
        c <- group iterator) yield c -> goldWord;

    pairs.toMap;
  }



}

  /*
def decodeCognates(inference: ParsimonyInference[factorFactory.Factors], tree: Tree):IndexedSeq[Cognate] = tree match {
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
