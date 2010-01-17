package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scala.collection.mutable.ArrayBuffer
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;

class Compression(klThreshold: Double) {
  import BigramSemiring._;
  import ring._;
  def handleAuto(auto: Automaton[Double,Int,Char]) = {
    auto.reweight(promote[Int] _ , promoteOnlyWeight _).cost.counts;
  }
  def compress(ao: Automaton[Double,Int,Char]) = {
    val counts : LogPairedDoubleCounter[Char,Char] = handleAuto(ao);
    val marginal = marginalize(counts);
    // special handling for # in the marginal case, since it represents the start token
    // but we want the end token.
    val endCount = logSum((for {
      (_,ctr) <- counts.rows;
      v = ctr('#')
    } yield v).toSeq);
    
    marginal('#') = endCount;

    val charIndex = Index[Char]();
    val divergentStates = Map.empty ++ (for {
      (ch,ctr) <- counts.rows;
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
      chreal = if(ch2 == '#') implicitly[Alphabet[Char]].epsilon else ch2
     } yield Arc(-1,idx2,chreal,w-marginal.logTotal);


    val divArcs = for {
      (ch1,ctr) <- divergentStates.iterator;
      idx1 = charIndex(ch1);
      (ch2,w) <- ctr.iterator;
      idx2 = if(ch2 == '#') -2
        else if(divergentStates contains ch2) charIndex(ch2)
        else -1
      chreal = if(ch2 == '#') implicitly[Alphabet[Char]].epsilon else ch2
     } yield Arc(idx1,idx2,chreal,w-ctr.logTotal);

    val auto = automaton(Map(startState->0.0),Map(endState->0.0))(
      (unigramArcs ++ divArcs).toSeq :_*
    );
    auto


    
  }
}

trait Compressor[State,T] {
  def destinationFor(state: State, trans: T): State
  def beginningUnigram: T
  protected def alphabet: Alphabet[T];
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