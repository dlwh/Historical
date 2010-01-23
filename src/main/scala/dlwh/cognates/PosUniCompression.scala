package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


abstract class PosUniCompression[@specialized("Char") T:Alphabet](maxLength: Int,
                                                                  chars: Set[T],
                                                                  val beginningUnigram: T) extends Compressor[Int,T] with ArcCreator[Int,T] {
  require(maxLength >= 1);

  protected def alphabet = implicitly[Alphabet[T]];
  
  def compress(auto: Automaton[Double,_,T]):Automaton[Double,Int, T] = {
    // Set up the semiring
    val tgs = new PositionalUnigramSemiring(maxLength, chars, beginningUnigram, cheatOnEquals=true);
    import tgs._;
    import ring._;

    try {
      val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
      compress(cost.totalProb,cost.decode);
    } catch {
      case e => println(tgs.charIndex); println(auto); throw e
    }
  }

  def destinationFor(i: Int, t: T) = i + 1;

  def compress(prob: Double, counts: Seq[LogDoubleCounter[T]]): Automaton[Double,Int,T] = {

    // 1 is always just #
    val arcs = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      arc <- arcsForCounter(i,ctr)
    } yield arc;


    val endingWeights = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex
      w = finalWeight(i,ctr);
      if w != Double.NegativeInfinity
    } yield (i,w);

    val startState = 0;

    val auto = Automaton.intAutomaton(Map(startState->prob),Map.empty ++ endingWeights)(
      (arcs).toSeq :_*
    );
    auto
  }
}
