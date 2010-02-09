package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scalala.Scalala._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


abstract class TriCompression[@specialized("Char") T:Alphabet](klThreshold: Double,
                                                      maxStates: Int,
                                                      intBigrams: Set[(T,T)],
                                                      val beginningUnigram: T)
                                                      extends Compressor[Seq[T],T] with ArcCreator[Seq[T],T] {
  import TrigramSemiring._;
  require(maxStates >= 1);

  def alphabet = implicitly[Alphabet[T]];
  
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
    val chars = Set.empty ++ (for( (k,v) <- bigrams.activeDomain.iterator; a <- 1 to 2 iterator) yield if (a == 1) k else v);

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

  type Statistics = (LogPairedDoubleCounter[Bigram[T],T],LogPairedDoubleCounter[T,T]);

  def gatherStatistics(chars: Set[T], auto: Automaton[Double,_,T]): (Statistics,Double) = {
    val tgs = new TrigramSemiring[T](chars,intBigrams,beginningUnigram,cheatOnEquals=true);
    import tgs._;
    import ring._;
    println("Enter");
    val cost = auto.reweight(promote[Any] _, promoteOnlyWeight _ ).cost;
    println("Exit");
    ( (cost.decode - cost.totalProb value,cost.decodeBigrams - cost.totalProb value),cost.totalProb);
  }

  val gramIndex = Index[Seq[T]]();


  def destinationFor(history: Seq[T], trans: T):Seq[T] = {
    var whole = history ++ Seq(trans);
    while(!gramIndex.contains(whole)) {
      whole = whole.drop(1);
    }
    gramIndex(whole);
    whole
  }


  def compress(prob: Double, stats: Statistics):Automaton[Double,Seq[T],T] = {
    val (counts,bigrams) = stats;
    val marginal = marginalizeCounts(bigrams);
    val selectedStates = selectStates(maxStates,marginal,bigrams,counts);
    //println(selectedStates);
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
