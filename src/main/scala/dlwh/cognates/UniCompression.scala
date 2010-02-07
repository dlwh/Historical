package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;
import scalala.Scalala._;


abstract class UniCompression[@specialized("Char") T:Alphabet](val beginningUnigram: T) extends Compressor[Unit,T] with ArcCreator[Unit,T] {
  def alphabet = implicitly[Alphabet[T]];
  type Statistics = LogDoubleCounter[T];
  def gatherStatistics(chars: Set[T], auto: Automaton[Double,_,T]) = {
    val tgs = new UnigramSemiring(chars, beginningUnigram);
    import tgs._;
    import ring._;

    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    (cost.decode,cost.totalProb);
  }

  def interpolate(a: Statistics, eta1: Double, b: Statistics, eta2: Double) = {
    val logEta = Math.log(eta2);
    val c = (a + Math.log(eta1)) value;
    for( (k,v) <- b) {
      c(k) = logSum(c(k),v + logEta);
    }
    c
  }

  def destinationFor(i: Unit, t: T) = i;

  def compress(prob: Double, counts: LogDoubleCounter[T]): Automaton[Double,Unit,T] = {

    // 1 is always just #
    val arcs = arcsForCounter( (), counts);

    val endingWeights = ( () -> finalWeight( () ,counts));
    val startState = ();

    val auto = Automaton.automaton(Map(startState->prob),Map.empty + endingWeights)(
      (arcs).toSeq :_*
    );
    auto
  }
}

class SafeUniCompression(val beginningUnigram: Char, expLength: Double) extends Compressor[Unit, Char] {
  def destinationFor(a: Unit, c: Char) = a;
  import scalanlp.fst.Alphabet._;
  val inner = new UniCompression(beginningUnigram) with NormalizedTransitions[Unit,Char];
  def alphabet = inner.alphabet;
  type Statistics = inner.Statistics;
  def gatherStatistics(chars: Set[Char], auto: Automaton[Double,_,Char]) = {
    val decayAuto = new DecayAutomaton(expLength, chars);
    val (stats,total) = inner.gatherStatistics(chars,auto & decayAuto);
    val retVal = LogDoubleCounter[Char]
    for( (k,v) <- stats) {
      if(k == beginningUnigram) 
        retVal(k) = v - decayAuto.stopProb;
      else 
        retVal(k) = v - decayAuto.arcCost;
    }
    // TODO: what to do with total
    (retVal,total)
  }

  def interpolate(a: Statistics, eta1: Double, b: Statistics, eta2: Double) = inner.interpolate(a,eta1,b,eta2);

  def compress(prob: Double, counts: Statistics) = {
    val auto = inner.compress(0.0,counts);
    val decayAuto = new DecayAutomaton(expLength, Set.empty ++ counts.keysIterator - beginningUnigram);
    val difference = prob - (auto & decayAuto cost)
    val ret = auto.scaleInitialWeights(difference);
    ret
  }
}