package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._;
import scala.collection.mutable.PriorityQueue



abstract class NewAgglomerative(tree: Tree, singletonBonus: Double = 0) {
  val factory: SuffStatsFactorsFactory;

  case class Item(groupA: CognateGroup, groupB: CognateGroup, priority: Double);
  implicit val itemOrdering = Ordering[Double].on((_:Item).priority);
  case class State(groups: IndexedSeq[CognateGroup], likelihood: Double,
                   factors: Map[Symbol,factory.Factors]);

  def score(g: CognateGroup, factors: factory.Factors) = {
    new ParsimonyInference(factors, tree, g.cognates.toIndexedSeq).likelihood
  }

  final def iterations(cognates: IndexedSeq[CognateGroup], factors: Map[Symbol,factory.Factors], groupsPerIteration: Int= 1): Iterator[State] = {
    println("????");
    new Iterator[State] {
      var state = initialState(cognates, factors);
      def hasNext = !pq.isEmpty;
      var emptyScores = state.groups.par.map { g =>
        val r = score(g,factors(g.gloss))
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
        val _score = score(a merge b, factors(a.gloss)) + { if(a.cognates.size == 1 || b.cognates.size == 1) singletonBonus else 0.0};
        println(a,b,_score,emptyScores(a),emptyScores(b), _score - emptyScores(a)-emptyScores(b));
        Item(a,b,_score - emptyScores(a)-emptyScores(b));
      }

      val pq: PriorityQueue[Item] = new PriorityQueue[Item]()(itemOrdering) ++= scores;
      println("PQQQ: " + pq.size);
      var toRemove = Set[CognateGroup]();

      def next = {
        var numMerged = 0;

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
  final def initialState(cognates: IndexedSeq[CognateGroup], factors: Map[Symbol, factory.Factors]):State = {
    State(cognates, Double.NegativeInfinity, factors);
  }
  final protected def nextState(oldState: State, newGroups: IndexedSeq[CognateGroup], ll: Double):State = {
    State(newGroups, ll, oldState.factors);
  }


  def successors(state: State, emptyScores: Map[CognateGroup,Double], g: CognateGroup, groups: Iterable[CognateGroup]) = {
    println("Successors");
    groups.toIndexedSeq.filter(g.canMerge _).par.map { b =>
      val bb = score(g merge b, state.factors(g.gloss));
      println("Suc",g,b,bb, emptyScores(g), emptyScores(b),bb - emptyScores(g) - emptyScores(b));
      Item(g, b, bb - emptyScores(g) - emptyScores(b))
    };
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
