package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scalanlp.counters.LogCounters.{logSum=>_,log=> _ ,_};
import scalanlp.math.Numerics._;
import scalanlp.math.Semiring.LogSpace._;
import scalala.Scalala._;

import Types._;

trait Compressor[State,T] {
  def destinationFor(state: State, trans: T): State
  def beginningUnigram: T
  def alphabet: Alphabet[T];
  type Statistics
  /**
   * @return statistics and the total probability accumulated.
   */
  def gatherStatistics(validChars: Set[T], auto: Automaton[Double,_,T]):(Statistics,Double)
  def interpolate(stats1: Statistics, eta1: Double, stats2: Statistics, eta2:Double):Statistics
  def transformCounts(stats: Statistics)(f: (T,Double)=>Double):Statistics;
  def smooth(stats: Statistics, counts: LogDoubleCounter[T]): Statistics

  final def compress(auto: Automaton[Double,_,T], validChars: Set[T]):Automaton[Double,State,T] = {
    val stats = gatherStatistics(validChars,auto);
    compress(stats._2,stats._1);
  }

  def compress(totalProb: Double, stats: Statistics): Automaton[Double,State,T];
}

abstract class BiCompression[@specialized("T") T:Alphabet](klThreshold: Double,
                                                      maxStates: Int,
                                                      val beginningUnigram: T)
                                                      extends Compressor[Option[T],T] with ArcCreator[Option[T],T] {
  import BigramSemiring._;
  require(maxStates >= 1);

  def alphabet = implicitly[Alphabet[T]];
  
  private def marginalizeCounts(bigrams: LogPairedDoubleCounter[T,T]) = {
    val marginals = LogDoubleCounter[T];
    for( (b,ctr) <- bigrams.rows;
      (ch,v) <- ctr) {
      if(b != beginningUnigram || ch != beginningUnigram) {
        marginals(ch) = logSum(marginals(ch),v);
      }
    }

    marginals;
  }
  
  private sealed abstract class State {
    val transitions: LogDoubleCounter[T];
    val score: Double;
    def chars : Option[T];
  }
  private case class UniState(ch: T, transitions: LogDoubleCounter[T], score: Double) extends State {
    def chars = Some(ch);
  }
  private case class NullState(transitions: LogDoubleCounter[T], score: Double) extends State {
    def chars = None;
  }

  private def selectStates(maxStates:Int,
                   marginal: LogDoubleCounter[T],
                   bigrams: LogPairedDoubleCounter[T,T]) = {
    def orderStates(g1: State, g2: State) = {
      g1.score < g2.score
    }
    implicit val stateOrdering = Ordering.fromLessThan(orderStates _ )

    val pq = new PriorityQueue[State]();
    def whine(ctr: LogDoubleCounter[T], marginal: LogDoubleCounter[T], kl: Double) = {
      if(kl.isNaN) {
        println(ctr.toString + ctr.logTotal);
        println(marginal.toString + marginal.logTotal);
        println(kl);
      }
    }

    for {
      (history,ctr) <- bigrams.rows
      kl = klDivergence(marginal,ctr) * Math.exp( ctr.logTotal - marginal.logTotal)
    //  if kl > klThreshold
    } {
      pq += UniState(history,ctr,kl);
    }

    val result = scala.collection.mutable.Map[Option[T],State]();
    result += (None -> NullState(marginal,Double.PositiveInfinity));
    while(result.size < maxStates && !pq.isEmpty) {
      val oldState = pq.dequeue();

      result += ((oldState.chars,oldState));
    }

    Map.empty ++ result;
  }

  type Statistics = LogPairedDoubleCounter[T,T];

  def smooth(stats: Statistics, counts: LogDoubleCounter[T]): Statistics = {
    val res = stats.copy;
    for( (_,ctr) <- res.rows; (k,v) <- counts) {
      ctr(k) = logSum(ctr(k),v);
    }
    res
  }

  def interpolate(a: Statistics, eta1:Double, b: Statistics, eta2: Double) = {
    val logEta = log(eta2);
    val c = (a + Math.log(eta1)) value;
    for( (k,v) <- b) {
      c(k) = logSum(c(k),v + logEta);
    }
    c
  }

  def transformCounts(stats: Statistics)(f: (T,Double)=>Double):Statistics = {
    val r = LogPairedDoubleCounter[T,T];
    for( (k,v) <- stats) {
      r(k) = f(k._2,v);
    }
    r;
  }

  def gatherStatistics(chars: Set[T], auto: Automaton[Double,_,T]): (Statistics,Double) = {
    // Set up the semiring
    val tgs = new BigramSemiring[T](chars,beginningUnigram,cheatOnEquals=true);
    import tgs._;
    import ring._;
    val cost = auto.reweight(promote[Any] _, promoteOnlyWeight _ ).cost;
    val ret = (cost.counts - cost.totalProb value,cost.totalProb);
    ret
  }

  val gramIndex = Index[Option[T]]();

  override def destinationFor(s: Option[T], trans: T):Option[T] = {
    var whole = Some(trans);
    if(gramIndex.contains(whole)) {
      whole
    } else {
      None;
    }
  }

  override def compress(prob: Double, bigrams: Statistics): Automaton[Double,Option[T],T] = {

    val marginal = marginalizeCounts(bigrams);
    val selectedStates = selectStates(maxStates,marginal,bigrams);
    //println(selectedStates);
    for( a <- selectedStates.keysIterator) {
      gramIndex.index(a);
    }

    // a few useful states
    val unigramState = gramIndex(None);
    /*
    val startState = {
      if(selectedStates contains Seq(beginningUnigram))
        gramIndex(Seq(beginningUnigram))
      else if(selectedStates contains Seq(beginningUnigram,beginningUnigram))
        gramIndex(Seq(beginningUnigram,beginningUnigram))
      else unigramState;
    }
    */
    
    val startState = {
      if(selectedStates contains Some(beginningUnigram))
        (Some(beginningUnigram))
      else None
    }

    val endingChars = beginningUnigram;

    val divArcs = for {
      (chars,state) <- selectedStates.iterator;
      arc <- arcsForCounter(chars,state.transitions)
    } yield arc;
    //  idx1 = gramIndex(chars);
   
    val finalWeights = for {
      (chars, state) <- selectedStates
      //idx1 = gramIndex(chars);
      finalScore = finalWeight(chars, state.transitions);
      if finalScore != Double.NegativeInfinity
    } yield (chars,finalScore);

    val auto = Automaton.automaton(Map(startState->prob),Map.empty ++ finalWeights)(
      (divArcs).toSeq :_*
    );
    auto
  }
}

class SafeCompression[S](val chars: Set[Char],
                         val inner: Compressor[S,Char] with NormalizedTransitions[S,Char],
                         expLength: Double) extends Compressor[S, Char] {
  val beginningUnigram = inner.beginningUnigram;
  def destinationFor(a: S, c: Char) = inner.destinationFor(a,c);
  import scalanlp.fst.Alphabet._;
  def alphabet = inner.alphabet;
  type Statistics = inner.Statistics;

  def gatherStatistics(chars: Set[Char], auto: Automaton[Double,_,Char]) = {
    val decayAuto = new DecayAutomaton(expLength, chars);
    val (stats,total) = inner.gatherStatistics(chars,auto & decayAuto);
    val retVal = inner.transformCounts(stats) { (k,v) =>
      if(k == beginningUnigram) v - decayAuto.stopProb;
      else if(k != implicitly[Alphabet[Char]].epsilon)
        v - decayAuto.arcCost;
      else v;
    }
    // TODO: what to do with total
    val ret = (retVal,total)
    ret;
  }

  def interpolate(a: Statistics, eta1: Double, b: Statistics, eta2: Double) = inner.interpolate(a,eta1,b,eta2);
  def smooth(a: Statistics, ctr: LogDoubleCounter[Char]) = inner.smooth(a,ctr);
  def transformCounts(a: Statistics)(f: (Char,Double)=>Double) = inner.transformCounts(a)(f);

  def compress(prob: Double, counts: Statistics) = {
    val auto = inner.compress(0.0,counts);
    val decayAuto = new DecayAutomaton(expLength, chars);
    val currentCost = (auto & decayAuto cost);
    val difference = prob - currentCost;
    val ret = auto.scaleInitialWeights(difference);
    ret
  }
}

import scalanlp.fst.Arc;
trait ArcCreator[S,T] { this : Compressor[S,T] =>
  def arcsForCounter(state: S, ctr: LogDoubleCounter[T]): Iterator[Arc[Double,S,T]];
  def finalWeight(state: S, ctr: LogDoubleCounter[T]): Double
}

trait NormalizedTransitions[S,T] extends ArcCreator[S,T] { this: Compressor[S,T] =>
  def arcsForCounter(state: S, ctr: LogDoubleCounter[T]) = {
    val normalizer = ctr.logTotal;
    assert(!normalizer.isNaN,ctr)
    for{
      (ch2,w) <- ctr.iterator
      dest = destinationFor(state,ch2);
      if (ch2 != beginningUnigram)
    } yield Arc(state,dest,ch2,w-normalizer);
  }

    def finalWeight(state: S, ctr: LogDoubleCounter[T]): Double = {
       ctr(beginningUnigram) - ctr.logTotal;
    }
}


trait NormalizedByFirstChar[S,T] extends ArcCreator[S,(T,T)] { this: Compressor[S,(T,T)] =>
  private val eps = alphabet.epsilon._1;

  def arcsForCounter(state: S, ctr: LogDoubleCounter[(T,T)]) = {
    val paired = LogPairedDoubleCounter[T,T]();
    require(!ctr.logTotal.isInfinite && !ctr.logTotal.isNaN);

    val insertWeights = new ArrayBuffer[Double]();
    for( ((ch1,ch2),w) <- ctr) {
      paired(ch1,ch2) = w;
      if(ch1 == eps)
        insertWeights += w;
    }

    val extraNormalizer = Math.log(1-Math.exp(logSum(insertWeights)-ctr.logTotal));
    for {
      (ch1,inner) <- paired.rows
      trueTotal = if(ch1 == eps) paired.logTotal else inner.logTotal - extraNormalizer;
      (ch2,w) <- inner.iterator
      if ((ch1,ch2) != beginningUnigram)
      dest = destinationFor(state,(ch1,ch2))
    } yield Arc(state,dest,(ch1,ch2),w-trueTotal);
  }

  def finalWeight(state: S, ctr: LogDoubleCounter[(T,T)]): Double = {
    val insertWeights = for ( ((a,b),w) <- ctr if a == eps) yield w;
    val score = logSum(insertWeights.toSeq);

    Math.log(1 - Math.exp(score-ctr.logTotal));
  }
}
