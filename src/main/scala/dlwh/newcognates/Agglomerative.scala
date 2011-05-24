package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._;
import scala.collection.mutable.PriorityQueue
import scalanlp.stats.sampling.Rand
import scalanlp.config.Configuration
import java.io.File
import scalanlp.fst._;
import fast.AutomatonFactory
import scalanlp.util.Index
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Vector;

class Agglomerative[MyAff<:AffinityScorer](affFactory: Map[Symbol,AffinityScorer.Factory[MyAff]], tree: Tree, singletonBonus: Double =0) {

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
        val bonus: Double = if(a.cognates.size == 1 || b.cognates.size == 1) singletonBonus else 0.0
        println(a,b,score,bonus,emptyScores(a),emptyScores(b), score + bonus - emptyScores(a)-emptyScores(b));
        Item(a,b,score +bonus - emptyScores(a)-emptyScores(b));
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
            pq ++= successors(state, emptyScores, merged, byGloss(a.cognates.head.gloss))
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

  def purity(gold: Map[Cognate,Int], cognates: Seq[CognateGroup]) = {
    val numberAssignedCorrectly = cognates.iterator.map { group =>
      val goldClusters: Iterator[Int] = group.cognates.iterator.map(c => gold(c));
      scalala.tensor.counters.Counters.count(goldClusters).max;
    } reduceLeft(_+_);
    val numGroups = gold.size;
    numberAssignedCorrectly * 1.0 / numGroups;
  }
}

/*
object RunWordAgglomerative {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val legalGloss = args(0).split(':').map(Symbol(_)).toSet;
    println(legalGloss);
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    val deathScore = math.log(config.readIn[Double]("initDeathProb"));

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new ResourceDataset(dataset_name,languages, withGloss);
    val basetree: Tree = dataset.tree;
    val tree = basetree.subtreeAt(config.readIn[String]("subtree",basetree.label));
    val leaves = tree.leaves;

    val cogs = dataset.cognates.filter(group => legalGloss(group.head.gloss));
    val alphabet = Index(cogs.iterator.flatMap(_.iterator).flatMap(_.word.iterator) ++ Iterator.single(implicitly[Alphabet[Char]].epsilon));

    val gold: IndexedSeq[Seq[Cognate]] = cogs.map(_.filter(cog => leaves(cog.language))).filterNot(_.isEmpty);
    println(gold.length);
    val goldTags = Accuracy.assignGoldTags(gold);
    val numGold = Accuracy.numGold(gold);

    val data = gold.flatten
    println(data.length);
    val randomized = data;
    val legalWords = randomized.toSet;

    import scalanlp.math.Semiring.LogSpace._;
    val ed = new ThreeStateEditDistance(alphabet);
    val factorFactory = new WordFactorsFactory(ed)
    import factorFactory.editDistance
    val legalByGloss: Map[Symbol, Set[String]] = legalWords.groupBy(_.gloss).mapValues( cognates => cognates.map(_.word));


    val learningEpochs = config.readIn[Int]("learningEpochs",0);
    val epochs = Iterator.iterate( (Map.empty[Language,editDistance.Parameters].withDefaultValue(editDistance.initialParameters),0)) { costsEpoch =>
      val (costs,epoch) = costsEpoch;
      println("Starting epoch " + epoch)

      def deathProbs(a: Language, b: Language) = deathScore;

      val mapped = legalByGloss.mapValues { legalWords =>
        val factors = new factorFactory.WordFactors(legalWords, costs);
        val scorerFactory = GlossRestrictedScorer.factory(SumScorer.factory(new DeathTreeScorer.Factory(tree,deathProbs),
          TreeEliminationScorer.factory(tree, factors)));
        scorerFactory
      }
      var lastLL = Double.NegativeInfinity;
      val bipartite = new Agglomerative(mapped,tree);
      val iter = bipartite.iterations(randomized);

      var lastS:bipartite.State = null;
      for( (s,iter) <- iter.zipWithIndex if s.likelihood > lastLL || epoch == learningEpochs) {
        val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, s.groups)
        lastLL = s.likelihood;
        lastS = s;
        val f1 = 2 * (precision * recall) / (precision + recall)
        println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + s.likelihood  + "\t" + f1 + "\t" + s.groups.length + "\t" + Accuracy.purity(goldTags,s.groups));
        if(s.groups.length == gold.length) println(":: ===============================================");
        println(s.groups)
      }

      val newMatrices = if(epoch >= learningEpochs) costs else {
        println("Training epoch " + epoch);
        val groupedByGloss = lastS.groups.groupBy(_.gloss);

        val allEdges = tree.edges;
        val start: Map[String, editDistance.SufficientStatistics] = (for( edge <- allEdges) yield {
          edge._2 -> editDistance.emptySufficientStatistics
        }) toMap


        val bigScores = groupedByGloss.foldLeft(start) {  (acc,pair) =>
          val (gloss,groups) = pair
          val legalWords = groups.flatMap(c => c.cognates.values.map(_.word)).toSet;
          val factors = new factorFactory.WordFactors(legalWords, costs);

          import editDistance.SufficientStatistics;

          val counts = groups.par(2).mapReduce({ (group) =>
            val inf = new TreeElimination(factors, tree, group);
            println(group.prettyString(tree) + " has score " + inf.likelihood);
            val edgeCounts = (for (pair@(parent, child) <- allEdges.iterator;
                                   marginal <- inf.edgeMarginal(parent, child).iterator)
            yield {child -> marginal.expectedCounts});
            val result = edgeCounts.toMap;
            result:Map[Language,SufficientStatistics]
          }, editDistance.sumCounts(_:Map[Language,SufficientStatistics],_:Map[Language,SufficientStatistics]))
          editDistance.sumCounts(acc,counts);
        }
        editDistance.makeParameters(bigScores);
      }

      val nextEpoch = epoch + 1
      (newMatrices -> nextEpoch);
    }

    epochs.drop(1).take(learningEpochs+1).foreach(identity);
  }

}
*/

object GoldStatus {
  def buildTree(t: Tree, groups: Map[Language,IndexedSeq[Cognate]], assign: Map[Cognate,Int]):Option[(Tree,Set[Int],Set[Int])] = t match {
    case t:Child => for( cognates <- groups.get(t.label)) yield {
      val gold = cognates.map(assign).toSet;
      val resultString = t.label + ": " + gold +  " " + cognates.map(_.word);
      (Child(resultString),gold,gold)
    }

    case Ancestor(label,children) =>
      val childrenWithInformation = children.flatMap(buildTree(_,groups,assign));
      if(childrenWithInformation.isEmpty) None
      else {
        val union = childrenWithInformation.foldLeft(Set.empty[Int])(_ ++ _._2);
        val inter = childrenWithInformation.map(_._3).reduceLeft(_ & _);
        val resultString = t.label + ": " + {if(inter.isEmpty) union else inter};
        Some((Ancestor(resultString,childrenWithInformation.map(_._1)),union,inter));
      }
  }

}
