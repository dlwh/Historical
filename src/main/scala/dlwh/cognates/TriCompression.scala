package dlwh.cognates;

import scala.collection.mutable.ArrayBuffer;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


class TriCompression(klThreshold: Double, maxStates: Int, chars: Set[(Char,Char)]) {
  
  // Set up the semiring
  val tgs = new TrigramSemiring(chars);
  import TrigramSemiring._;
  import tgs._;
  import ring._;
  
  require(maxStates >= 1);
  
  private def computeDistance(auto: Transducer[Double,_,Char,Char]) = {
    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    (cost.totalProb,cost.decode);
  }

  private def marginalizeCounts(counts: LogPairedDoubleCounter[Bigram,(Char,Char)]) = {
    val marginals = LogDoubleCounter[(Char,Char)];
    val bigrams = LogPairedDoubleCounter[(Char,Char),(Char,Char)];
    for( (Bigram(b1,b2),ctr) <- counts.rows;
      (ch,v) <- ctr) {
      if(b2 != (('#','#')) || ch != (('#','#'))) {
        marginals(ch) = logSum(marginals(ch),v);
        bigrams(b2,ch) = logSum(bigrams(b2,ch),v);
      }
    }

    (marginals,bigrams);
  }
  
  private sealed abstract class State {
    val transitions: LogDoubleCounter[(Char,Char)];
    val score: Double;
    def chars : Seq[(Char,Char)]
  }
  private case class BiState(bigram: Bigram, transitions: LogDoubleCounter[(Char,Char)], score: Double) extends State {
    def chars = Seq(bigram._1,bigram._2); 
  }
  private case class UniState(ch: (Char,Char), transitions: LogDoubleCounter[(Char,Char)], score: Double) extends State {
    def chars = Seq(ch);
  }
  private case class NullState(transitions: LogDoubleCounter[(Char,Char)], score: Double) extends State {
    def chars = Seq.empty;
  }

  private def selectStates(maxStates:Int,
                   marginal: LogDoubleCounter[(Char,Char)],
                   bigrams: LogPairedDoubleCounter[(Char,Char),(Char,Char)],
                   trigrams: LogPairedDoubleCounter[Bigram,(Char,Char)]) = {
    def orderStates(g1: State, g2: State) = {
      g1.score < g2.score
    }
    implicit val stateOrdering = Ordering.fromLessThan(orderStates _ )

    val pq = new PriorityQueue[State]();

    for {
      (history,ctr) <- bigrams.rows;
      kl = klDivergence(ctr,marginal)
       () = println(history + " " + kl);
      if kl > klThreshold
    } {
      pq += UniState(history,ctr,kl);
    }

    val result = scala.collection.mutable.Map[Seq[(Char,Char)],State]();
    while(result.size < maxStates-1 && !pq.isEmpty) {
      val oldState = pq.dequeue();

      result += ((oldState.chars,oldState));

      oldState match {
        case UniState(ch,trans,_) if ch != (('#','#')) =>
        // compute successors, enqueue them.
        for {
          ch2 <- chars;
          bg = Bigram(ch2,ch);
          bgTrans = trigrams(bg);
          // todo: state split
          kl = klDivergence(bgTrans,oldState.transitions)
          () = println(bg + " " + kl);
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

  def compress(ao: Transducer[Double,_,Char,Char]) = {
    val (prob,counts) = computeDistance(ao);
    val (marginal,bigrams) = marginalizeCounts(counts);

    val selectedStates = selectStates(maxStates,marginal,bigrams,counts);
    println(selectedStates);

    val gramIndex = Index[Seq[(Char,Char)]]();
    gramIndex.index(Seq.empty);
    for( a <- selectedStates.keysIterator) {
      gramIndex.index(a);
    }

    // a few useful states

    val endState = -1;
    val unigramState = 0;
    val startState = {
      if(selectedStates contains Seq(beginningUnigram))
        gramIndex(Seq(beginningUnigram))
      else if(selectedStates contains Seq(beginningUnigram,beginningUnigram))
        gramIndex(Seq(beginningUnigram,beginningUnigram))
      else unigramState;
    }
    
    val endingChars = encode('#','#');
    def destinationFor(history: Seq[(Char,Char)], trans: (Char,Char)):Int = {
      if(trans == endingChars) {
        endState 
      } else {
        var whole = history ++ Seq(trans);
        while(!gramIndex.contains(whole)) {
          whole = whole.drop(1);
        }
        gramIndex(whole);
      }
    }


    val unigramArcs = for {
      (ch2,w) <- marginal.iterator;
      idx2 = destinationFor(Seq.empty,ch2);
      (decCh1,decCh2) = decode(ch2)
      ch1real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh1
      ch2real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh2
    } yield Arc(unigramState,idx2,ch1real,ch2real,w-marginal.logTotal);


    val divArcs = for {
      (chars,state) <- selectedStates.iterator;
      idx1 = gramIndex(chars);
      (ch2,w) <- state.transitions.iterator
      idx2 = destinationFor(chars,ch2);
      (decCh1,decCh2) = decode(ch2)
      ch1real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh1
      ch2real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh2
    } yield Arc(idx1,idx2,ch1real,ch2real,w-state.transitions.logTotal);

    val auto = Transducer.transducer(Map(startState->prob),Map(endState->0.0))(
      (unigramArcs ++ divArcs).toSeq :_*
    );
    auto
  }
}
