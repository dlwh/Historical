package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


abstract class UniCompression[@specialized("Char") T:Alphabet](chars: Set[T],
                                                               val beginningUnigram: T) extends Compressor[Unit,T] with ArcCreator[Unit,T] {

  protected def alphabet = implicitly[Alphabet[T]];

  def compress(auto: Automaton[Double,_,T]):Automaton[Double,Unit, T] = {
    // Set up the semiring
    val tgs = new UnigramSemiring(chars, beginningUnigram);
    import tgs._;
    import ring._;

    try {
      val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
      compress(cost.totalProb,cost.decode);
    } catch {
      case e => println(tgs.charIndex); throw e
    }
  }

  def destinationFor(i: Unit, t: T) = i;

  def compress(prob: Double, counts: LogDoubleCounter[T]): Automaton[Double,Unit,T] = {

    // 1 is always just #
    val arcs = arcsForCounter( (), counts);

    val endingWeights = ( () -> finalWeight(() ,counts));
    val startState = ();

    val auto = Automaton.automaton(Map(startState->prob),Map.empty + endingWeights)(
      (arcs).toSeq :_*
    );
    auto
  }
}
