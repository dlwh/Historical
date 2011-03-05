package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalala.tensor.counters.LogCounters.{logSum => _, _};
import scalala.Scalala._;
import scala.collection.mutable.PriorityQueue;
import scala.reflect.OptManifest;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


abstract class PosUniCompression(maxLength: Int, val beginningUnigram: Char)(implicit alpha: Alphabet[Char], man:
                                      OptManifest[Char]) extends Compressor[Int,Char] with ArcCreator[Int,Char] {
  require(maxLength >= 1);

  def alphabet = implicitly[Alphabet[Char]];

  type Statistics = Seq[LogDoubleCounter[Char]];

  def gatherStatistics(validChars: Set[Char], auto: Automaton[Double,_,Char]):(Statistics,Double) = {
    val tgs = new PositionalUnigramSemiring(maxLength, validChars, beginningUnigram, cheatOnEquals=true);
    import tgs._;
    import ring._;
    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    (cost.decode,cost.totalProb);
  }

  def interpolate(stats1: Statistics, eta1: Double, stats2: Statistics, eta2 :Double):Statistics = {
    val logEta = math.log(eta2);
    for ( (a,b) <- stats1 zip stats2) yield {
      val c = (a + math.log(eta1)) value;
      for( (k,v) <- b) {
        c(k) = logSum(c(k),v + logEta);
      }
      c
    }
  }

  def transformCounts(stats: Statistics)(f: (Char,Double)=>Double):Statistics = {
    val r = Seq.fill(stats.length)(LogDoubleCounter[Char]);
    for((ctr,i) <- stats zipWithIndex;
        (k,v) <- ctr) {
      r(i)(k) = f(k,v);
    }
    r;
  }

  
  def smooth(stats: Statistics, counts: LogDoubleCounter[Char]): Statistics = {
    val res = stats.map(_.copy);
    for( ctr <- res; (k,v) <- counts) {
      ctr(k) = logSum(ctr(k),v);
    }
    res;
  }

  def compress(chars: Set[Char], auto: Automaton[Double,_,Char]):Automaton[Double,Int, Char] = {
    val (stats,cost) = gatherStatistics(chars,auto);
    compress(cost,stats);
  }

  def destinationFor(i: Int, t: Char) = (i + 1) min (maxLength-2);

  def compress(prob: Double, counts: Seq[LogDoubleCounter[Char]]): Automaton[Double,Int,Char] = {

    // 1 is always just #
    val arcs = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      if ctr.logTotal > Double.NegativeInfinity;
      arc <- arcsForCounter(i,ctr)
      _ = assert(!arc.weight.isNaN)
    } yield arc;


    val endingWeights = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      if ctr.logTotal > Double.NegativeInfinity;
      w = finalWeight(i,ctr);
      _ = assert(!w.isNaN)
      if w != Double.NegativeInfinity
    } yield (i,w);

    val startState = 0;

    val auto = Automaton.intAutomaton(Map(startState->prob),Map.empty ++ endingWeights)(
      (arcs).toSeq :_*
    );
    auto
  }
}
