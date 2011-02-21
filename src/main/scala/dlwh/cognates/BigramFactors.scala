package dlwh.cognates;

import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;
import scalala.Scalala._;
import scalala.tensor.counters.LogCounters._;

import Types._;

/**
 * Factors where all transducers are projected to bigrams
 */
class BigramFactors extends Factors {
  type Marginal = BigramMarginal
  type EdgeFactor = BigramEdge
  def edgeFor(parent: Language, child: Language, alphabet: Set[Char]) = new EdgeFactor();
  def rootMarginal(alphabet: Set[Char]) = new Marginal(Set.empty);
  def marginalForWord(word: String, score: Double= 0.0) = new Marginal(extractBigrams(word));
  def product(upward: Boolean, ms: Seq[Marginal]) = ms.reduceLeft(_*_);

  class BigramEdge extends EdgeFactorBase {
    def childMarginalize(c: Marginal): Marginal = c;
    def parentMarginalize(c: Marginal): Marginal = c;
    def withMarginals(a: Marginal, b: Marginal) = this;
  }

  class BigramMarginal(val bigrams: Set[(Char,Char)]) extends MarginalBase {
    def *(other: Marginal) = {
      new BigramMarginal(this.bigrams ++ other.bigrams);
    }

    def apply(w: String) = {
      val wB = extractBigrams(w);
      2.0 * (wB & this.bigrams size) / (wB.size + bigrams.size);
    }

    def partition = bigrams.size toDouble;
  }

  def extractBigrams(word: String) = {
    Set.empty ++ (word.take(word.length-1) zip word.drop(1))
  }
}
