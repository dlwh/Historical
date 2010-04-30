/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package dlwh.cognates

import scala.collection.mutable.ArrayBuffer
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalanlp.fst.Arc;
import scalanlp.math.Numerics.logSum;

trait ArcCreator[S,T] { this : Compressor[S,T] =>
  def arcsForCounter(state: S, ctr: LogDoubleCounter[T]): Iterator[Arc[Double,S,T]];
  def finalWeight(state: S, ctr: LogDoubleCounter[T]): Double
}

/** For automata */
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


/** For edit distance transducers */
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
