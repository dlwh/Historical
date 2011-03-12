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

class Agglomerative[MyAff<:AffinityScorer](affFactory: Map[Symbol,AffinityScorer.Factory[MyAff]], tree: Tree) {

  case class Item(groupA: CognateGroup, groupB: CognateGroup, priority: Double);
  implicit val itemOrdering = Ordering[Double].on((_:Item).priority);
  case class State(groups: IndexedSeq[CognateGroup], likelihood: Double,
                   affScorer: Map[Symbol,MyAff]);

  final def iterations(cognates: IndexedSeq[Cognate], groupsPerIteration: Int= 1): Iterator[State] = {
    new Iterator[State] {
      var state = initialState(cognates);
      def hasNext = !pq.isEmpty;
      var emptyScores = state.groups.par.map { g =>
        val r = state.affScorer(g.gloss)(g,CognateGroup.empty);
        println(g->r);
        g -> r
      }.toMap;
      state = state.copy(likelihood = emptyScores.values.reduceLeft(_+_));
      var byGloss = state.groups.groupBy(_.glosses.head).mapValues(_.toSet);
      val toConsider = byGloss.filter(_._2.size > 1).values.toIndexedSeq.map { (groups:Iterable[CognateGroup]) =>
        val gg = groups.toIndexedSeq;
        for(i <- 0 until gg.size;
            a = gg(i);
            j <- (i+1) until gg.size;
            b = gg(j) if a canMerge b) yield {
          (a,b)
        }
      }.flatten

      val scores = toConsider.par.map { case(a,b) =>
        val score = state.affScorer(a.gloss)(a,b);
        println(a,b,score,emptyScores(a),emptyScores(b), score - emptyScores(a)-emptyScores(b));
        Item(a,b,score - emptyScores(a)-emptyScores(b));
      }



      val pq = new PriorityQueue[Item]()(itemOrdering) ++= scores;
      var toRemove = Set[CognateGroup]();

      def next = {
        var numMerged = 0;
        import scala.util.control.Breaks._;
        var ll = state.likelihood;
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
    State(cognates.map(CognateGroup(_)), Double.NegativeInfinity, affFactory.mapValues(_.initialScorer).toMap);
  }
  final protected def nextState(oldState: State, newGroups: IndexedSeq[CognateGroup], ll: Double):State = {
    State(newGroups, ll, oldState.affScorer.map { case (gloss,scorer) => gloss -> affFactory(gloss).nextScorer(scorer, newGroups)});
  }


  def successors(state: State, emptyScores: Map[CognateGroup,Double], g: CognateGroup, groups: Iterable[CognateGroup]) = {
    val cal = state.affScorer(g.gloss).calibrate(g);
    println("Successors");
    groups.toIndexedSeq.filter(g.canMerge _).par.map { b =>
      val bb = cal(b);
      println("Suc",g,b,bb, emptyScores(g), emptyScores(b),bb - emptyScores(g) - emptyScores(b));
      Item(g, b, bb - emptyScores(g) - emptyScores(b))
    };
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

    val r= (numCorrect * 1.0 / numGuesses,numCorrect * 1.0 / numGold);
    println(r,numGuesses,numGold);
    r
  }
}

object RunWordAgglomerative {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val legalGloss = args(0).split(':').map(Symbol(_)).toSet;
    println(legalGloss);
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    val deathScore = math.log(config.readIn[Double]("initDeathProb"));
    val initSub = config.readIn[Double]("initSub");
    val initDel = config.readIn[Double]("initDel");

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val cogs = dataset.cognates.filter(group => legalGloss(group.head.gloss));
    println(cogs.length);

    val gold: IndexedSeq[Seq[Cognate]] = cogs.map(_.filter(cog => leaves(cog.language)))
    val goldTags = Accuracy.assignGoldTags(gold);
    val numGold = Accuracy.numGold(gold);

    val data = gold.flatten
    println(data.length);
    val randomized = data;
    val legalWords = randomized.toSet;
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);
    println(alphabet);

    import scalanlp.math.Semiring.LogSpace._;
    val autoFactory = new AutomatonFactory(Index(alphabet + implicitly[Alphabet[Char]].epsilon));
    val factorFactory = new WordFactorsFactory(autoFactory)
    import factorFactory.factory._;


    def deathProbs(a: Language, b: Language) = deathScore;

    val legalByGloss: Map[Symbol, Set[String]] = legalWords.groupBy(_.gloss).mapValues( cognates => cognates.map(_.word));

    val mapped = legalByGloss.mapValues { legalWords =>
      val factors = new factorFactory.WordFactors(legalWords, initSub, initDel);
      val scorerFactory = GlossRestrictedScorer.factory(SumScorer.factory(new DeathTreeScorer.Factory(tree,deathProbs),
        TreeEliminationScorer.factory(tree, factors)));
      scorerFactory
    }
    val bipartite = new Agglomerative(mapped,tree);
    val iter = bipartite.iterations(randomized);
    for( (s,iter) <- iter.zipWithIndex) {

      val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, s.groups)
      println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + s.likelihood  + "\t" + 2 * (precision * recall)/(precision + recall));
      println(s.groups)
    }
  }

}
