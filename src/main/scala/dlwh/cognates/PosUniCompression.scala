package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


class PosUniCompression[@specialized("Char") T:Alphabet](maxLength: Int, chars: Set[T], beginningUnigram: T) {
  require(maxLength >= 1);
  
  def compress(auto: Automaton[Double,_,T]):Automaton[Double,Int, T] = {
    // Set up the semiring
    val tgs = new PositionalUnigramSemiring(maxLength, chars, beginningUnigram);
    import tgs._;
    import ring._;

    println("Enter");
    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    println("Exit");
    compress(cost.totalProb,cost.decode);
  }

  def compress(prob: Double, counts: Seq[LogDoubleCounter[T]]): Automaton[Double,Int,T] = {

    // 1 is always just #
    val arcs = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      (ch, w) <- ctr.iterator
      if ch != beginningUnigram
    } yield Arc(i,i+1,ch,w - ctr.logTotal);


    val endingWeights = Map.empty ++ (for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      (ch, w) <- ctr.iterator
      if ch == beginningUnigram
    } yield (i,w - ctr.logTotal)) withDefaultValue(Double.NegativeInfinity);


    val startState = 0;

    val auto = Automaton.intAutomaton(Map(startState->prob),endingWeights)(
      (arcs).toSeq :_*
    );
    auto
  }
}
