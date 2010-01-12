package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


class TriCompression[@specialized("Char") T:Alphabet](klThreshold: Double, maxStates: Int, chars: Set[T], intBigrams: Set[(T,T)], beginningUnigram: T) {
  import TrigramSemiring._;
  require(maxStates >= 1);
  
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
    def chars : Seq[T]
  }
  private case class BiState(bigram: Bigram[T], transitions: LogDoubleCounter[T], score: Double) extends State {
    def chars = Seq(bigram._1,bigram._2); 
  }
  private case class UniState(ch: T, transitions: LogDoubleCounter[T], score: Double) extends State {
    def chars = Seq(ch);
  }
  private case class NullState(transitions: LogDoubleCounter[T], score: Double) extends State {
    def chars = Seq.empty;
  }

  private def selectStates(maxStates:Int,
                   marginal: LogDoubleCounter[T],
                   bigrams: LogPairedDoubleCounter[T,T],
                   trigrams: LogPairedDoubleCounter[Bigram[T],T]) = {
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

    val result = scala.collection.mutable.Map[Seq[T],State]();
    result += (Seq.empty -> NullState(marginal,Double.PositiveInfinity));
    while(result.size < maxStates && !pq.isEmpty) {
      val oldState = pq.dequeue();

      result += ((oldState.chars,oldState));

      oldState match {
        case UniState(ch,trans,_) if ch != beginningUnigram =>
        // compute successors, enqueue them.
        for {
          ch2 <- chars + beginningUnigram
          bg = Bigram(ch2,ch);
          bgTrans = trigrams(bg);
          if bgTrans.size != 0
          // todo: state split
          kl = klDivergence(bgTrans,oldState.transitions) * Math.exp( bgTrans.logTotal - marginal.logTotal);
          () = println(bg + " " + kl + " " + bgTrans.logTotal);
          if kl > klThreshold
        } {
          pq += BiState(bg,bgTrans,kl);
        }
        case BiState(bg,bgTrans,kl) =>
        // We have to add the previous state, if it's not already there.
        // We should dequeue it and process it immediately, or something
        // but meh.
        val uniOneMinus = Seq(bg._1);
        if (!result.contains(uniOneMinus)) {
          result += (uniOneMinus -> UniState(bg._1,bigrams(bg._1),-1));
        }
          
        case _ => () // do nothing
      }
    }

    Map.empty ++ result;
  }

  def compress(auto: Automaton[Double,_,T]):Automaton[Double,Seq[T], T] = {
    // Set up the semiring
    val tgs = new TrigramSemiring[T](chars,intBigrams,beginningUnigram);
    import tgs._;
    import ring._;

    println("Enter");
    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    println("Exit");
    compress(cost.totalProb,cost.decode,cost.decodeBigrams);
  }

  def compress(prob: Double,
               counts: LogPairedDoubleCounter[Bigram[T],T],
               bigrams: LogPairedDoubleCounter[T,T]): Automaton[Double,Seq[T],T] = {

    val marginal = marginalizeCounts(bigrams);
    val selectedStates = selectStates(maxStates,marginal,bigrams,counts);
    //println(selectedStates);

    val gramIndex = Index[Seq[T]]();
    for( a <- selectedStates.keysIterator) {
      gramIndex.index(a);
    }

    // a few useful states
    val unigramState = gramIndex(Seq.empty);
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
      if(selectedStates contains Seq(beginningUnigram))
        (Seq(beginningUnigram))
      else Seq.empty;
    }

    val endingChars = beginningUnigram;
    def destinationFor(history: Seq[T], trans: T):Seq[T] = {
      var whole = history ++ Seq(trans);
      while(!gramIndex.contains(whole)) {
        whole = whole.drop(1);
      }
      gramIndex(whole);
      whole
    }

    val divArcs = for {
      (chars,state) <- selectedStates.iterator;
    //  idx1 = gramIndex(chars);
      (ch2,w) <- state.transitions.iterator
      idx2 = destinationFor(chars,ch2);
      if (ch2 != beginningUnigram)
    //} yield Arc(idx1,idx2,ch2,w-state.transitions.logTotal);
    } yield Arc(chars,idx2,ch2,w-state.transitions.logTotal);

    val finalWeights = for {
      (chars, state) <- selectedStates
      idx1 = gramIndex(chars);
      finalScore = state.transitions(beginningUnigram);
      if finalScore != Double.NegativeInfinity
    } yield (chars,finalScore - state.transitions.logTotal);

    val auto = Automaton.automaton(Map(startState->prob),Map.empty ++ finalWeights)(
      (divArcs).toSeq :_*
    );
    auto
  }
}
