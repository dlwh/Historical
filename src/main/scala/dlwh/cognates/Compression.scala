package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;

trait Compressor[State,T] {
  def destinationFor(state: State, trans: T): State
  def beginningUnigram: T
  protected def alphabet: Alphabet[T];
}

abstract class BiCompression[@specialized("Char") T:Alphabet](klThreshold: Double,
                                                      maxStates: Int,
                                                      chars: Set[T],
                                                      val beginningUnigram: T)
                                                      extends Compressor[Option[T],T] with ArcCreator[Option[T],T] {
  import BigramSemiring._;
  require(maxStates >= 1);

  protected def alphabet = implicitly[Alphabet[T]];
  
  private def marginalizeCounts(bigrams: LogPairedDoubleCounter[T,T]) = {
    val marginals = LogDoubleCounter[T];
    for( (b,ctr) <- bigrams.rows;
      (ch,v) <- ctr) {
      if(b != (('#','#')) || ch != (('#','#'))) {
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

    for {
      (history,ctr) <- bigrams.rows;
      kl = klDivergence(ctr,marginal) * Math.exp( ctr.logTotal - marginal.logTotal);
      () = println(history + " " + kl);
      if kl > klThreshold
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

  def compress(auto: Automaton[Double,_,T]):Automaton[Double,Option[T], T] = {
    // Set up the semiring
    val tgs = new BigramSemiring[T](chars,beginningUnigram,cheatOnEquals=true);
    import tgs._;
    import ring._;

    println("Enter");
    val cost = auto.reweight(promote[Any] _, promoteOnlyWeight _ ).cost;
    println("Exit");
    compress(cost.totalProb,cost.counts);
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

  def compress(prob: Double,
               bigrams: LogPairedDoubleCounter[T,T]): Automaton[Double,Option[T],T] = {

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
    } yield (chars,finalScore - state.transitions.logTotal);

    val auto = Automaton.automaton(Map(startState->prob),Map.empty ++ finalWeights)(
      (divArcs).toSeq :_*
    );
    auto
  }
}

import scalanlp.fst.Arc;
trait ArcCreator[S,T] { this : Compressor[S,T] =>
  def arcsForCounter(state: S, ctr: LogDoubleCounter[T]): Iterator[Arc[Double,S,T]];
  def finalWeight(state: S, ctr: LogDoubleCounter[T]): Double
}

trait NormalizedTransitions[S,T] extends ArcCreator[S,T] { this: Compressor[S,T] =>
  def arcsForCounter(state: S, ctr: LogDoubleCounter[T]) = {
    for{
      (ch2,w) <- ctr.iterator
      dest = destinationFor(state,ch2);
      if (ch2 != beginningUnigram)
    } yield Arc(state,dest,ch2,w-ctr.logTotal);
  }

    def finalWeight(state: S, ctr: LogDoubleCounter[T]): Double = {
       ctr(beginningUnigram) - ctr.logTotal;
    }
}


trait NormalizedByFirstChar[S,T] extends ArcCreator[S,(T,T)] { this: Compressor[S,(T,T)] =>
  private val eps = alphabet.epsilon._1;

  def arcsForCounter(state: S, ctr: LogDoubleCounter[(T,T)]) = {
    val paired = LogPairedDoubleCounter[T,T]();

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
