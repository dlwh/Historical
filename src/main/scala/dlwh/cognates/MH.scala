package dlwh.cognates

import scalanlp.fst._
import scalanlp.stats.sampling.Rand;
import scala.collection.mutable.ArrayBuffer
import scalala.Scalala._;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import Types._;
import scalanlp.util.Log._;


class MH(tree: Tree, cognates: Seq[Cognate], languages: Set[String], alpha: Double = 0.7, batchSize: Int = 3, nIter:Int = 100) {
  require(alpha < 1 && alpha >= 0.5);
  require(batchSize >= 1);

  val initialMatchCounts = 80.0;
  val initialSubCounts = 5.0;
  val initialInsCounts = 1.0;

  private val alphabet = Set.empty ++ cognates.iterator.flatMap(_.word.iterator);
  private val eps = implicitly[Alphabet[Char]].epsilon;
  val transCompr = new UniCompression[(Char,Char)]( ('#','#') ) with NormalizedByFirstChar[Unit,Char];
  type Statistics = Map[(Language,Language),transCompr.Statistics];

  private val edgesToLearn = tree.edges.toSeq;

  private val allPairs = for {
    a <- alphabet + eps;
    b <- alphabet + eps;
    if a != eps || b != eps
  } yield (a,b);

  def eta(iter: Int) = Math.pow(iter+2,-alpha);

  case class Table(members: Map[Language,Word], factors: TransducerFactors) {
    lazy val likelihood = io.likelihood;
    def io = new InsideOutside(tree,factors,members);
    def remove(l: Language) = this.copy(members = this.members - l);
    def include(l: Language, w: Word) = this.copy(members = this.members.updated(l,w));
  }

  def proposeSwap(assignments: Seq[Table], current: Int) = {
    Rand.choose(assignments zipWithIndex).filter(_._2 != current).get;
  }
  def likelihood(factors: TransducerFactors, group: Table) = {
    group.likelihood;
  }

  def prior(table: Table) = 0.0;

  case class State(assignments: Map[Cognate,Int],
                   ios: Seq[Table],
                   factors: TransducerFactors) {

    def likelihood = ios.iterator.map(_.likelihood).reduceLeft(_+_);

    def evaluateSwap(language: Language, from: Int, to: Int):Option[State] = {
      globalLog.log(INFO)("GC in" + memoryString);
      System.gc();
      globalLog.log(INFO)("GC out" + memoryString);
      val fromIO = ios(from);
      val fromWord = fromIO.members.get(language);
      val toIO = ios(to);
      val toWord = toIO.members.get(language);
      val newFrom = {
        val pruned = fromIO.remove(language)
        if(!toWord.isEmpty) pruned.include(language,toWord.get);
        else pruned
      }

      val newTo = {
        val pruned = toIO.remove(language)
        if(!fromWord.isEmpty) pruned.include(language,fromWord.get);
        else pruned;
      }

      println("Considering " + fromIO.members  +" vs. "+ toIO.members + " for lang" + language);
      val old = fromIO.likelihood + toIO.likelihood + prior(fromIO) + prior(toIO);
      val new_ = newFrom.likelihood + newTo.likelihood + prior(newFrom) + prior(newTo);
      if( new_ - old > Math.log(Rand.uniform.get))  {
        var newAssignments = assignments;
        fromWord foreach { w => 
          newAssignments = assignments.updated(Cognate(w,language),to);
        }
        toWord foreach { w =>
          newAssignments = assignments.updated(Cognate(w,language),from);
        }
        assert(newAssignments.size == assignments.size,(newAssignments.size,assignments.size));
        val newTables = ios.updated(from,newFrom).updated(to,newTo);
        println("Success!")
        Some(this.copy(assignments = newAssignments, ios = newTables));
      } else {
        println("Failure");
        None
      }
    }

    def step(word: Cognate) = {
      val from = assignments(word);
      val to = proposeSwap(ios,from)._2;
      val newState : Option[State] = evaluateSwap(word.language,from,to);
      newState
    }

  }

  def initialState(words: Seq[Cognate], factors: TransducerFactors) = {
    val nextGroup = collection.mutable.Map() ++= languages.map( _ -> 0);
    val tables = new ArrayBuffer[Set[Cognate]];
    val assignments = collection.mutable.Map[Cognate,Int]();
    for( w <- words) {
      val nG = nextGroup(w.language);
      nextGroup(w.language) += 1;
      if(tables.length == nG) {
        tables += Set(w);
      } else {
        tables(nG) += w;
      }
      assignments(w) = nG;
    }
    val ios = for( set <- tables) yield set.foldLeft(new Table(Map.empty,factors))( (t,c) => t.include(c.language,c.word));
    State(Map.empty ++ assignments, ios, factors);
  }

  def iterations = {
    val factors = new TransducerFactors(tree,alphabet) with PosUniPruning;
    Iterator.iterate(initialState(cognates,factors)) { startState =>
      var ll = 0.0;
      val wordsState = cognates.foldLeft( (startState) ) { (assignedState,word) =>
        assignedState.step(word).getOrElse(assignedState);
      }
      wordsState
     }
   }

  def oneBest(a: Psi) = {
    import scalanlp.math.Semiring.LogSpace._;
    val obring = new OneBestSemiring[Char,Double];
    import obring.{ring,promote,promoteOnlyWeight};
    a.reweight(promote _, promoteOnlyWeight _ ).cost
  }

  def gatherStatistics(ios: Iterator[InsideOutside[TransducerFactors]]) = {
    val trigramStats = for{
      io <- ios
      pair@ (fromL,toL) <- edgesToLearn.iterator;
      trans <- io.edgeMarginal(fromL, toL).iterator
    } yield {
      val alphabet = io.alphabet;
      val allPairs = for {
        a <- alphabet + eps;
        b <- alphabet + eps;
        if a != eps || b != eps
      } yield (a,b);

      globalLog.log(INFO)("BG in" + memoryString);
      println(pair);
      val cost = transCompr.gatherStatistics(allPairs,trans.fst);
      globalLog.log(INFO)("BG out" + memoryString);
      globalLog.log(INFO)("GC2 in" + memoryString);
      System.gc();
      globalLog.log(INFO)("GC2 out" + memoryString);

      (fromL,toL) -> cost._1;
    }

    import collection.mutable.{Map=>MMap}
    val stats = MMap[(Language,Language),transCompr.Statistics]();
    for ( (lpair,ctr) <- trigramStats)  {
      stats(lpair) = stats.get(lpair).map(transCompr.interpolate(_,1,ctr,1)).getOrElse(ctr);
    }

    Map.empty ++ stats;
  }

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      langs -> transCompr.interpolate(as,1-eta,bs,eta);
    }

    newStats;
  }

  def mkFactors(statistics: Statistics) = {
    globalLog.log(INFO)("Trans in " + memoryString);
    val transducers = Map.empty ++ statistics.mapValues ( ctr =>  transCompr.compress(0.0,ctr));

    val factors = new TransducerFactors(tree,alphabet,transducers) with PosUniPruning;
    globalLog.log(INFO)("Trans out " + memoryString);
    factors
  }
}


object RunMH {
  def main(args: Array[String]) {
    val dataset = new Dataset(args(0),args(1).split(","));
    val data = dataset.cognates.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val iter = new MH(dataset.tree, randomized, Set.empty ++ args(1).split(",")).iterations;
    for( state <- iter.take(1000)) {
      state.ios.map(_.members) foreach println;
    }
  }
}
