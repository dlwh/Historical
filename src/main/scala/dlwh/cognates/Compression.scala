package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalanlp.counters.LogCounters._;

class Compression(klThreshold: Double) {
  import BigramSemiring._;
  import ring._;
  def handleAuto(auto: Automaton[Double,Int,Char]) = {
    auto.reweight(promote[Int] _ , promoteOnlyWeight _).cost.counts;
  }
  def compress(ao: Automaton[Double,Int,Char]) = {
    val cts : LogPairedDoubleCounter[Char,Char] = handleAuto(ao);
    val marginal = marginalize(cts);
    val charIndex = Index[Char]();
    val divergentStates = Map.empty ++ (for {
      (ch,ctr) <- cts.rows;
      kl = klDivergence(ctr,marginal)
      () = println(kl);
      if kl > klThreshold
    } yield (ch,ctr));

    import scalanlp.math.Semiring.LogSpace._;
    import Automaton._;
    val startState = if(divergentStates contains '#') charIndex('#') else -1;
    val endState = -2;

    val unigramArcs = for {
      (ch2,w) <- marginal.iterator;
      idx2 = if(ch2 == '#') -2
        else if(divergentStates contains ch2) charIndex(ch2)
        else -1
      chreal = if(ch2 == '#') ao.inAlpha.epsilon else ch2
     } yield Arc(-1,idx2,chreal,chreal,w-marginal.logTotal);


    val divArcs = for {
      (ch1,ctr) <- divergentStates.iterator;
      idx1 = charIndex(ch1);
      (ch2,w) <- ctr.iterator;
      idx2 = if(ch2 == '#') -2
        else if(divergentStates contains ch2) charIndex(ch2)
        else -1
      chreal = if(ch2 == '#') ao.inAlpha.epsilon else ch2
     } yield Arc(idx1,idx2,chreal,chreal,w-ctr.logTotal);

    val auto = automaton(Map(startState->0.0),Map(endState->0.0))(
      (unigramArcs ++ divArcs).toSeq :_*
    );
    auto


    
  }
}
