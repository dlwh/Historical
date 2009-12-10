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
  private def handleAuto(auto: Transducer[Double,_,Char,Char]) = {
    println("Enter");
    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    println("Exit");
    (cost.totalProb,cost.decode);
  }

  private def marginalizeCounts(counts: LogPairedDoubleCounter[Gram,EncodedChars]) = {
    val result = LogDoubleCounter[EncodedChars];
    for( (Unigram(_),ctr) <- counts.rows;
      (ch,v) <- ctr) {
      result(ch) = logSum(result(ch),v);
    }

    result;
  }

  private def selectStates(maxStates:Int,
                   counts: LogPairedDoubleCounter[Gram,EncodedChars],
                   comparisonGram: Gram=>LogDoubleCounter[EncodedChars]) = {
    def orderGrams(g1: (Gram,LogDoubleCounter[EncodedChars],Double),
                   g2: (Gram,LogDoubleCounter[EncodedChars],Double)) = (
      g1._3 < g2._3
    )
    implicit val tupleOrdering = Ordering.fromLessThan(orderGrams _ )

    val pq = new PriorityQueue[(Gram,LogDoubleCounter[EncodedChars],Double)]();

    for {
      (gram:Unigram,ctr) <- counts.rows;
      comparison = comparisonGram(gram);
      kl = klDivergence(ctr,comparison)
      //    () = println(kl);
      if kl > klThreshold
    } {
      pq += ((gram,ctr,kl));
    }

    val result = new ArrayBuffer[(Gram,LogDoubleCounter[EncodedChars])];
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

  def compress(ao: Transducer[Double,_,Char,Char]) = {
    val (prob,counts) = handleAuto(ao);
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
      if(selectedStates contains beginningUnigram)
        gramIndex(beginningUnigram)
      else if(selectedStates contains beginningBigram)
        gramIndex(beginningBigram)
      else -1;
    }


    val endingChars = encode('#','#');
    // compute destination for an arc 
    def destinationFor(gram: Gram, ch2: EncodedChars) = {
      if(ch2 == endingChars) -2
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
      (decCh1,decCh2) = decode(ch2)
      ch1real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh1
      ch2real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh2
    } yield Arc(unigramState,idx2,ch1real,ch2real,w-marginal.logTotal);

    // mainloop: until we have enough states, add arcs for that state, also enqueue
    // successor states (bigrams)
    val divArcs = for {
      (gram,ctr) <- selectedStates.iterator;
      idx1 = gramIndex(gram);
      (ch2,w) <- ctr.iterator;
      idx2 = destinationFor(gram,ch2);
      (decCh1,decCh2) = decode(ch2)
      ch1real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh1
      ch2real = if(decCh1 == '#' && decCh2 == '#') ao.inAlpha.epsilon else decCh2
    } yield Arc(idx1,idx2,ch1real,ch2real,w-ctr.logTotal);

    val auto = Transducer.transducer(Map(startState->prob),Map(endState->0.0))(
      (unigramArcs ++ divArcs).toSeq :_*
    );
    auto
  }
}
