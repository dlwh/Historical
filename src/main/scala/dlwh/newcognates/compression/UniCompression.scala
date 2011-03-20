package dlwh.newcognates.compression

import scalanlp.fst._;
import scalala.tensor.counters.LogCounters.{logSum=> _, _};
import Automaton._;
import scalala.Scalala._;


abstract class UniCompression[T:Alphabet](val beginningUnigram: T) extends Compressor[Unit,T] with ArcCreator[Unit,T] {
  def alphabet = implicitly[Alphabet[T]];
  type Statistics = LogDoubleCounter[T];
  def gatherStatistics(chars: Set[T], auto: Automaton[Double,_,T]) = {
    val tgs = new UnigramSemiring(chars, beginningUnigram);
    import tgs._;
    import ring._;

    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    (cost.decode - cost.totalProb value,cost.totalProb);
  }

  def transformCounts(stats: Statistics)(f: (T,Double)=>Double):Statistics = {
    val r = LogDoubleCounter[T];
    for( (k,v) <- stats) {
      r(k) = f(k,v);
    }
    r;
  }

  def interpolate(a: Statistics, eta1: Double, b: Statistics, eta2: Double) = {
    val c = LogDoubleCounter[T]();
    for( (k,v) <- a) {
      c(k) = v + math.log(eta1);
    }
    for( (k,v) <- b) {
      c(k) = logSum(c(k),v + math.log(eta2));
    }
    c
  }

  def smooth(stats: Statistics, counts: LogDoubleCounter[T]): Statistics = {
    val ctr = stats.copy;
    for( (k,v) <- counts) {
      ctr(k) = logSum(ctr(k),v);
    }
    ctr
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
