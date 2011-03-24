package dlwh.newcognates

import scalanlp.math.Numerics
import scalala.tensor.dense.DenseVector
import scalanlp.util.{Encoder, Index}
import scalala.tensor.sparse.SparseVector

/**
 * 
 * @author dlwh

class LanguageModelFactorsFactory(charIndex: Index[Char]) extends SuffStatsFactorsFactory {
  val pe = new AlignmentPairEncoder(charIndex);

  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters):Factors

  def optimize(suffStats: Map[Language, SufficientStatistics]):Map[Language,EdgeParameters] = {

  }


  def emptySufficientStatistics = null

  def initialParameters :EdgeParameters=  new EdgeParameters {
    val theScore = -math.log(charIndex.size);
    def score(prev: Char, next: Char) =  theScore;
  }

  trait EdgeParameters {
    def score(prev: Char, next: Char):Double
  }
  case class SufficientStatistics(transitions: Vector)

  class Factors(legalWords: Index[Word], parameters: Language=>EdgeParameters) extends FactorsWithSuffStats {
    val wordIndex = Index(legalWords);
    def initialBelief(lang: Language) = new Belief(allOnes);

    def initialMessage(from: Language, to: Language) = new Belief(allOnes);

    def beliefForWord(w: Word) = new Belief({
      val r = Encoder.fromIndex(wordIndex).mkDenseVector(Double.NegativeInfinity);
      r(wordIndex(w)) = 0.0;
      r
    })
    val allOnes = new DenseVector(wordIndex.size);

    val rootMessage = {
      val allOnes = new DenseVector(wordIndex.size);
      new Belief(logNormalizeInPlace(allOnes));
    }

    def logNormalizeInPlace(v: DenseVector) = {
      v -= Numerics.logSum(v);
      v
    }

    def edgeFor(parent: Language, child: Language) = {
      new Edge(paremeters(child));
    }

    case class Belief(beliefs: DenseVector) extends BeliefBase {
      lazy val partition = { val r = Numerics.logSum(beliefs); assert(!r.isNaN & !r.isInfinite); r}
      def apply(word: String)= beliefs(wordIndex(word));

      def /(b: Belief):Belief = {
        val diff = beliefs -b.beliefs value;
        new Belief(diff);
      }

      def *(b: Belief):Belief = {
        val r = new Belief(beliefs + b.beliefs value);
        r
      }

      override def toString() = ("Belief: " + Encoder.fromIndex(wordIndex).decode(beliefs));

      def normalized = scaleBy(-partition)

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score value);
        new Belief(scaled);
      }

    }

    case class Edge(costs: DenseVector, counts: Int=>SparseVector, child: Option[Belief]=None) extends EdgeBase with HasSufficientStatistics {
      def score(a: Word, b: Word) = costs(wordIndex(b));

      def sufficientStatistics = {
        val child = this.child.get;
        var c = 0;
        while(c < child.size) {

          c += 1;
        }
      }
    }

  }
}
 */

