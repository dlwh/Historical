package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._;
import scala.collection.mutable.PriorityQueue
import scalanlp.stats.sampling.Rand
import scalanlp.config.Configuration
import java.io.File
import scalanlp.fst._;
import dlwh.cognates._
import fast.AutomatonFactory
import scalanlp.util.Index

class Agglomerative[MyAff<:AffinityScorer](affFactory: AffinityScorer.Factory[MyAff], tree: Tree) {
  case class Item(groupA: CognateGroup, groupB: CognateGroup, priority: Double);
  implicit val itemOrdering = Ordering[Double].on((_:Item).priority);
  case class State(groups: IndexedSeq[CognateGroup], likelihood: Double,
                   affScorer: MyAff);

  final def iterations(cognates: IndexedSeq[Cognate], groupsPerIteration: Int= 1): Iterator[State] = {
    new Iterator[State] {
      var state = initialState(cognates);
      def hasNext = true
      var emptyScores = state.groups.par.map { g => g -> state.affScorer(g,CognateGroup.empty)}.toMap;
      state = state.copy(likelihood = emptyScores.values.reduceLeft(_+_));
      var byGloss = state.groups.groupBy(_.glosses.head).mapValues(_.toSet);
      val scores = byGloss.filter(_._2.size > 1).values.toIndexedSeq.map { (groups:Iterable[CognateGroup]) =>
        val gg = groups.toIndexedSeq;
        for(i <- 0 until gg.size;
            a = gg(i);
            cal = state.affScorer.calibrate(a);
            j <- (i+1) until gg.size;
            b = gg(j) if a canMerge b) yield {
          val score = cal(b);
          println(a,b,score,emptyScores(a),emptyScores(b), score - emptyScores(a)-emptyScores(b));
          Item(a,b,score - emptyScores(a)-emptyScores(b));
        }
      }

      val pq = new PriorityQueue[Item]()(itemOrdering) ++= scores.flatten;
      var toRemove = Set[CognateGroup]();

      def next = {
        var numMerged = 0;
        import scala.util.control.Breaks._;
        var ll = state.likelihood;
        println(pq.filter(item => !toRemove(item.groupA) && !toRemove(item.groupB)).mkString("PQ","\n","ENDQP"));
        while(!pq.isEmpty && numMerged < groupsPerIteration) {
          val Item(a,b,score) = pq.dequeue;
          if(!toRemove(a) && !toRemove(b)) {
            toRemove += a
            toRemove += b;
            val merged =  (a merge b);
            numMerged += 1;
            println("merge " + a.prettyString(tree) + "\n with " + b.prettyString(tree) + "\n Merge Score:" + score)
            println("Result: " + merged.prettyString(tree));
            ll += score;
            emptyScores += (merged -> (score + emptyScores(a) + emptyScores(b)));
            emptyScores -= a
            emptyScores -= b;
            byGloss = byGloss.updated(a.glosses.head, byGloss(a.glosses.head) - a - b + merged);
            pq ++= successors(state, emptyScores, merged, byGloss(a.cognates.values.head.gloss))
          }
        }


        val newGroups =  emptyScores.keySet.toIndexedSeq;
        state = nextState(state, newGroups,ll);
        state
      }
    }

  }
  final def initialState(cognates: IndexedSeq[Cognate]):State = {
    State(cognates.map(CognateGroup(_)), Double.NegativeInfinity, affFactory.initialScorer)
  }
  final protected def nextState(oldState: State, newGroups: IndexedSeq[CognateGroup], ll: Double):State = {
    State(newGroups, ll, affFactory.nextScorer(oldState.affScorer, newGroups))
  }


  def successors(state: State, emptyScores: Map[CognateGroup,Double], g: CognateGroup, groups: Iterable[CognateGroup]) = {
    val cal = state.affScorer.calibrate(g);
    println("Successors");
    groups.toIndexedSeq.filter(g.canMerge _).par.map { b =>
      val bb = cal(b);
      println("Suc",g,b,bb, emptyScores(g), emptyScores(b),bb - emptyScores(g) - emptyScores(b));
      Item(g, b, bb - emptyScores(g) - emptyScores(b))
    };
  }

}

object RunAgglomerative {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val cogs = dataset.cognates
    println(cogs.length);

    val gold = cogs.map(_.filter(cog => leaves(cog.language))).take(2)

    val data = gold.flatten
    println(data.length);
    val randomized = Rand.permutation(data.length).draw().map(data);
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);
    println(alphabet);

//    val factors = new BigramFactors;
    val compressor = readAutomataCompressor(config, "transducers.message");
    def editDistance(l: Language, l2: Language)= new EditDistance(-10,-10,alphabet);
    def initBelief(l: Language) = new DecayAutomaton(1, alphabet);
    def initMessage(a: Language, b: Language) = new DecayAutomaton(40, alphabet);
    val factors = new TransducerFactors(alphabet, compressor, initBelief(""), editDistance _,  initMessage _);

    val scorerFactory = GlossRestrictedScorer.factory(TreeScorer.factory(tree, factors));
    val bipartite = new Agglomerative(scorerFactory, tree);
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

object Accuracy {
  def assignGoldTags(gold: Seq[Seq[Cognate]]): Map[Cognate, Int] = {
    (for((group,index) <- gold.zipWithIndex; c <- group) yield (c,index)) toMap
  }

  def numGold(gold: Seq[Seq[Cognate]]): Int = {
    gold.foldLeft(0)( (acc,set) => acc + (set.size) * (set.size- 1) / 2);
  }

  def precisionAndRecall(gold: Map[Cognate,Int], numGold: Int, cognates: Seq[CognateGroup]) = {
    var numCorrect = 0;
    var numGuesses = 0;
    for(group <- cognates) {
      val gs = group.cognates.values.toIndexedSeq;
      for(i <- 0 until gs.length; j <- (i+1) until gs.length) {
        if(gold(gs(i)) == gold(gs(j)))
          numCorrect += 1
        numGuesses += 1
      }
    }

    (numCorrect * 1.0 / numGuesses,numCorrect * 1.0 / numGold);
  }
}

object RunNewAgglomerative {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val gloss = Symbol(args(0));
    println(gloss);
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val cogs = dataset.cognates.filter(_.head.gloss == gloss);
    println(cogs.length);

    val gold = cogs.map(_.filter(cog => leaves(cog.language)))
    val goldTags = Accuracy.assignGoldTags(gold);
    val numGold = Accuracy.numGold(gold);

    val data = gold.flatten
    println(data.map(_.gloss).toSet);
    println(data.length);
    val randomized = Rand.permutation(data.length).draw().map(data);
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);
    println(alphabet);

    import scalanlp.math.Semiring.LogSpace._;
    val autoFactory = new AutomatonFactory(Index(alphabet + implicitly[Alphabet[Char]].epsilon));
    val factorFactory = new FastTransducerFactory {
      val factory = autoFactory;
      val model = new factory.PositionalUnigramModel(10);
    }
    import factorFactory.factory._;

    def editDistance(l: Language, l2: Language)= new EditDistance(-5,-6);
    def initBelief(l: Language) = new MaxLengthAutomaton(9);//new DecayAutomaton(1);
    def initMessage(a: Language, b: Language) = new UnigramModel;
    val rootBelief = new DecayAutomaton(4);
    val factors = new factorFactory.FastTransducerFactors(rootBelief, initBelief _, editDistance _,  initMessage _);

    val scorerFactory = GlossRestrictedScorer.factory(TreeScorer.factory(tree, factors));
    val bipartite = new Agglomerative(scorerFactory,tree);
    val iter = bipartite.iterations(randomized);
    for( (s,iter) <- iter.take(100).zipWithIndex) {
      val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, s.groups)
      println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + s.likelihood  + "\t" + numGold);
      println(s.groups)
    }
  }

}


