package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;

class TriCompression(klThreshold: Double) {
  import TrigramSemiring._;
  import ring._;
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

  def compress(ao: Automaton[Double,Int,Char]) = {
    val counts : LogPairedDoubleCounter[Gram,Char] = handleAuto(ao);
    val marginal = marginalizeCounts(counts);

    // compare to the gram of order n-1
    def comparisonGram(gram: Gram) = gram match {
      case b: Bigram => counts(Unigram(b._2)) 
      case u: Unigram => marginal;
      case _ => error("Unexpected" + gram);
    }

    val gramIndex = Index[Gram]();
    val divergentStates = Map.empty ++ (for {
      (gram,ctr) <- counts.rows;
      if gram != Noncegram
      comparison = comparisonGram(gram);
      kl = klDivergence(ctr,comparison)
      () = println(kl);
      if kl > klThreshold
    } yield (gram,ctr));

    import scalanlp.math.Semiring.LogSpace._;
    import Automaton._;
    val startState = {
      if(divergentStates contains Unigram('#'))
        gramIndex(Unigram('#'))
      else if(divergentStates contains Bigram('#','#'))
        gramIndex(Bigram('#','#'))
      else -1;
    }
    val endState = -2;
    val unigramState = -1

    // compute destination for an arc 
    def destinationFor(gram: Gram, ch2: Char) = {
      if(ch2 == '#') -2
      else if(gram == Noncegram) {
        if(divergentStates contains Unigram(ch2)) gramIndex(Unigram(ch2));
        else unigramState
      } else {
        val bg = gram match {
          case b: Bigram => Bigram(b._2,ch2);
          case u: Unigram => Bigram(u._1,ch2);
          case _ => error("Shouldn't be here"); 
        }
        if(divergentStates contains bg) gramIndex(bg);
        else if(divergentStates contains Unigram(ch2)) gramIndex(Unigram(ch2));
        else unigramState
      }
    }

    val unigramArcs = for {
      (ch2,w) <- marginal.iterator;
      idx2 = destinationFor(Noncegram,ch2);
      chreal = if(ch2 == '#') ao.inAlpha.epsilon else ch2
    } yield Arc(unigramState,idx2,chreal,chreal,w-marginal.logTotal);

    val divArcs = for {
      (gram,ctr) <- divergentStates.iterator;
      idx1 = gramIndex(gram);
      (ch2,w) <- ctr.iterator;
      idx2 = destinationFor(gram,ch2)
      chreal = if(ch2 == '#') ao.inAlpha.epsilon else ch2
     } yield Arc(idx1,idx2,chreal,chreal,w-ctr.logTotal);

    val auto = automaton(Map(startState->0.0),Map(endState->0.0))(
      (unigramArcs ++ divArcs).toSeq :_*
    );
    auto
    
  }
}
