package dlwh.cognates;

import scalanlp.math.Semiring;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;
import scalanlp.math.Numerics._;
import scalanlp.counters._;
import scalanlp.counters.Counters._;
import scalala.Scalala._;
import scalanlp.counters.LogCounters._;

import Types._;

class BigramFactors(logSmoothing: Double, alphabet: Set[Char]) extends Factors {
  type Marginal = BigramMarginal
  type EdgeFactor = BigramEdge
  def edgeFor(parent: Language, child: Language, alphabet: Set[Char]) = new EdgeFactor();
  def rootMarginal(alphabet: Set[Char]) = new Marginal(LogDoubleCounter[String]());
  def marginalForWord(w: String, score: Double= 0.0) = new Marginal(extractBigrams(w,score));

  class BigramEdge extends EdgeFactorBase {
    def childMarginalize(c: Marginal): Marginal = c;
    def parentMarginalize(c: Marginal): Marginal = c;
  }

  private val totalSmoothing = logSmoothing + Math.log(alphabet.size);

  class BigramMarginal(private val counter: LogDoubleCounter[String]) extends MarginalBase {
    def *(other: Marginal) = {
      new BigramMarginal(counter + other.counter value);
    }

    def apply(w: String) = {
      var score = 0.0;
      for( (gram,count) <- extractBigrams(w,score)) {
        score += count * (counter(gram) - partition);
      }
      score
    }

    def partition = logSum(counter.logTotal,totalSmoothing)
  }

  def extractBigrams(w: String, score: Double) = {
    val bigrams = for( a@(c1,c2) <- w.zip(w.drop(1))) yield { 
        (c1+""+c2,1.0)
    }
    val counts = Counters.aggregate(bigrams:_*);
    val lc = LogCounters.logNormalize(counts);
    lc.default = logSum(lc.default,logSmoothing);
    lc -= score
    lc
  }
}
