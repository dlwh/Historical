package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._;
import scala.collection.mutable.PriorityQueue
import scalanlp.stats.sampling.Rand
import scalanlp.config.Configuration
import java.io.File
import scalanlp.fst._;
import dlwh.cognates._

class Agglomerative[MyAff<:AffinityScorer](affFactory: AffinityScorer.Factory[MyAff]) {
  case class Item(groupA: CognateGroup, groupB: CognateGroup, priority: Double);
  implicit val itemOrdering = Ordering[Double].on((_:Item).priority);
  case class State(groups: IndexedSeq[CognateGroup],
                   affScorer: MyAff);

  final def iterations(cognates: IndexedSeq[Cognate], groupsPerIteration: Int= 10): Iterator[State] = {
    new Iterator[State] {
      var state = initialState(cognates);
      def hasNext = true
      var emptyScores = state.groups.par.map { g => g -> state.affScorer(g,CognateGroup.empty)}.toMap;
      var byGloss = state.groups.groupBy(_.glosses.head).mapValues(_.toSet);
      val scores = byGloss.filter(_._2.size > 1).values.toIndexedSeq.par.map { (groups:Iterable[CognateGroup]) =>
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
        breakable {
          for( Item(a,b,score) <- pq if !toRemove(a) && !toRemove(b)) {
            toRemove += a
            toRemove += b;
            val merged =  (a merge b);
            numMerged += 1;
            println("merge " +a + " " + b + " " + score)
            emptyScores += (merged -> (score + emptyScores(a) + emptyScores(b)));
            emptyScores -= a
            emptyScores -= b;
            byGloss = byGloss.updated(a.glosses.head, byGloss(a.glosses.head) - a - b + merged);
            pq ++= successors(state, emptyScores, merged, byGloss(a.cognates.values.head.gloss))
            if(numMerged == groupsPerIteration) break;
          }
        }


        val newGroups =  emptyScores.keySet.toIndexedSeq;
        state = nextState(state, newGroups);
        state
      }
    }

  }
  final def initialState(cognates: IndexedSeq[Cognate]):State = {
    State(cognates.map(CognateGroup(_)), affFactory.initialScorer)
  }
  final protected def nextState(oldState: State, newGroups: IndexedSeq[CognateGroup]):State = {
    State(newGroups, affFactory.nextScorer(oldState.affScorer, newGroups));
  }


  def successors(state: State, emptyScores: Map[CognateGroup,Double], g: CognateGroup, groups: Iterable[CognateGroup]) = {
    val cal = state.affScorer.calibrate(g);
    println("Successors");
    groups.toIndexedSeq.filter(g.canMerge _).par.map { b => println("Suc",g,b,cal(b), emptyScores(g), emptyScores(b)); Item(g, b, cal(b) - emptyScores(g) - emptyScores(b))};
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
    val bipartite = new Agglomerative(scorerFactory);
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

