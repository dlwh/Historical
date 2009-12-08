package dlwh.cognates;

import scala.collection.mutable.ArrayBuffer;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import scala.collection.mutable.PriorityQueue;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


class TriCompression(klThreshold: Double, maxStates: Int) {
  import TrigramSemiring._;
  import ring._;
  require(maxStates >= 1);
  def handleAuto(auto: Automaton[Double,Int,Char]) = {
    auto.reweight(promote[Int] _ , promoteOnlyWeight _).cost.counts;
  }

  def marginalizeCounts(counts: LogPairedDoubleCounter[Gram,Char]) = {
    val result = LogDoubleCounter[Char];
    for( (Unigram(_),ctr) <- counts.rows;
      (ch,v) <- ctr) {
      result(ch) = logSum(result(ch),v);
    }

    result;
  }

  def selectStates(maxStates:Int,
                   counts: LogPairedDoubleCounter[Gram,Char],
                   comparisonGram: Gram=>LogDoubleCounter[Char]) = {
    def orderGrams(g1: (Gram,LogDoubleCounter[Char],Double),
                   g2: (Gram,LogDoubleCounter[Char],Double)) = (
      g1._3 < g2._3
    )
    implicit val tupleOrdering = Ordering.fromLessThan(orderGrams _ )

    val pq = new PriorityQueue[(Gram,LogDoubleCounter[Char],Double)]();

    for {
      (gram:Unigram,ctr) <- counts.rows;
      comparison = comparisonGram(gram);
      kl = klDivergence(ctr,comparison)
  //    () = println(kl);
      if kl > klThreshold
    } {
      pq += ((gram,ctr,kl));
    }

    val result = new ArrayBuffer[(Gram,LogDoubleCounter[Char])];
    while(result.size < maxStates-1 && !pq.isEmpty) {
      val (gram,ctr,_) = pq.dequeue();

      result += ((gram,ctr));

      gram match {
        case Unigram(ch) =>
        // TODO: make this more efficient by precomputing successors
        for {
          (gram2 @ Bigram(`ch`,ch2),ctr) <- counts.rows;
          comparison = comparisonGram(gram);
          kl = klDivergence(ctr,comparison)
          //() = println(kl);
          if kl > klThreshold
        } {
          pq += ((gram2,ctr,kl));
        }
        case _ => () // do nothing
      }
    }

    Map.empty ++ result
  }

  def compress(ao: Automaton[Double,Int,Char]) = {
    val counts : LogPairedDoubleCounter[Gram,Char] = handleAuto(ao);
    val marginal = marginalizeCounts(counts);

    // compare to the gram of order n-1
    def comparisonGram(gram: Gram) = gram match {
      case b: Bigram => counts(Unigram(b._2)) 
      case u: Unigram => marginal;
      case _ => error("Unexpected" + gram);
    }

    val selectedStates = selectStates(maxStates,counts, comparisonGram _);

    val gramIndex = Index[Gram]();

    // a few useful states

    val endState = -2;
    val unigramState = -1
    // Compute the start state last
    val startState = {
      if(selectedStates contains Unigram('#'))
        gramIndex(Unigram('#'))
      else if(selectedStates contains Bigram('#','#'))
        gramIndex(Bigram('#','#'))
      else -1;
    }


    // compute destination for an arc 
    def destinationFor(gram: Gram, ch2: Char) = {
      if(ch2 == '#') -2
      else if(gram == Noncegram) {
        if(selectedStates contains Unigram(ch2)) gramIndex(Unigram(ch2));
        else unigramState
      } else {
        val bg = gram match {
          case b: Bigram => Bigram(b._2,ch2);
          case u: Unigram => Bigram(u._1,ch2);
          case _ => error("Shouldn't be here"); 
        }
        if(selectedStates contains bg) gramIndex(bg);
        else if(selectedStates contains Unigram(ch2)) gramIndex(Unigram(ch2));
        else unigramState
      }
    }

    val unigramArcs = for {
      (ch2,w) <- marginal.iterator;
      idx2 = destinationFor(Noncegram,ch2);
      chreal = if(ch2 == '#') ao.inAlpha.epsilon else ch2
    } yield Arc(unigramState,idx2,chreal,chreal,w-marginal.logTotal);

    // mainloop: until we have enough states, add arcs for that state, also enqueue
    // successor states (bigrams)
    var numStates = 1;
    val divArcs = for {
      (gram,ctr) <- selectedStates.iterator;
      idx1 = gramIndex(gram);
      (ch2,w) <- marginal.iterator;
      idx2 = destinationFor(gram,ch2);
      chreal = if(ch2 == '#') ao.inAlpha.epsilon else ch2
    } yield Arc(idx1,idx2,chreal,chreal,w-marginal.logTotal);

    val auto = automaton(Map(startState->0.0),Map(endState->0.0))(
      (unigramArcs ++ divArcs).toSeq :_*
    );
    auto
  }
}
