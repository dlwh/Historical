package dlwh.newcognates

import scalanlp.math.Numerics
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.Scalala._
import scalala.tensor.Vector
import scalanlp.util.{Lazy, Encoder, Index}
;

/**
 * 
 * @author dlwh
 **/

class LanguageModelFactorsFactory(charIndex: Index[Char]) extends SuffStatsFactorsFactory {
  val pe = new AlignmentPairEncoder(charIndex);

  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters):Factors = new Factors(Index(legalWords),edgeParameters);

  def optimize(suffStats: Map[Language, SufficientStatistics]):Map[Language,EdgeParameters] = {
    val allCounts = suffStats.values.foldLeft(new DenseVector(charIndex.size * charIndex.size)) { (v,v2) => v += v2.transitions; v};
    suffStats.mapValues { stats =>
      val totals = new Array[Double](charIndex.size);
      val counts = allCounts * .5 + stats.transitions value;
      for( (k,v) <- counts.activeElements) {
        val (prev,_) = pe.decode(k);
        totals(prev) += v;
      }

      new EdgeParameters {
        def apply(prev: Char, next: Char) = {
          val r = math.log(counts(pe.encode(prev,next)) + 0.01) - math.log(totals(charIndex(prev)) + .01 * charIndex.size);
          assert(!r.isNaN, counts(pe.encode(prev,next)) -> (totals(charIndex(prev)) + .1 * charIndex.size));
          r
        }
      }

    }
  }


  lazy val emptySufficientStatistics = new SufficientStatistics(new SparseVector(charIndex.size * charIndex.size));

  val initialParameters :EdgeParameters=  new EdgeParameters {
    val theScore = -math.log(charIndex.size);
    def apply(prev: Char, next: Char) =  theScore;
  }

  trait EdgeParameters {
    def apply(prev: Char, next: Char):Double
  }
  case class SufficientStatistics(transitions: Vector) extends BaseSufficientStatistics {
    def +(stats: SufficientStatistics) = SufficientStatistics(this.transitions + stats.transitions)

    def *(weight: Double) = SufficientStatistics(this.transitions * weight value);
  }

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

    import collection.{mutable=>m}
    private val precomputedECounts = new m.HashMap[Language,(Int)=>Lazy[SparseVector]] with m.SynchronizedMap[Language,(Int)=>Lazy[SparseVector]];
    private val precomputedCosts = new m.HashMap[Language,(Int)=>Lazy[Double]] with m.SynchronizedMap[Language,(Int)=>Lazy[Double]];

    def edgeFor(parent: Language, child: Language) = {
      val counts = precomputedECounts.getOrElseUpdate(child,computeECounts(parameters(child))) andThen (_.result);
      val scores = precomputedCosts.getOrElseUpdate(child,computeCosts(parameters(child))) andThen (_.result);
      new Edge(scores, counts);
    }

    private def computeCosts(parameters: EdgeParameters) = {
      Array.tabulate(wordIndex.size) { c =>
        Lazy.delay {
          val w = wordIndex.get(c);
          var cost = 0.0;
          var ch = '\0';
          var i = 0;
          while(i < w.length) {
            val chn = w(i);
            cost += parameters(ch,chn)
            ch = chn;
            i += 1
          }
          cost += parameters(ch,'\0')
          cost
        }
      }
    }

    private def computeECounts(parameters: EdgeParameters) = {
      Array.tabulate(wordIndex.size) { c =>
        Lazy.delay {
          val w = wordIndex.get(c);
          val result = new SparseVector(charIndex.size * charIndex.size,w.length + 1);
          var ch = '\0';
          var i = 0;
          while(i < w.length) {
            val chn = w(i);
            result(pe.encode(ch,chn)) += 1;
            ch = chn;
            i += 1
          }
          result(pe.encode(ch,'\0')) += 1;
          result;
        }
      }
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

    case class Edge(costs: Int=>Double, counts: Int=>SparseVector, child: Option[Belief]=None) extends EdgeBase with HasSufficientStatistics {
      def score(a: Word, b: Word) = costs(wordIndex(b));

      def sufficientStatistics = {
        val child = this.child.get.beliefs;
        var c = 0;
        val suffStats = new SparseVector(charIndex.size * charIndex.size);
        while(c < child.size) {
          if(child(c) != Double.NegativeInfinity) {
            suffStats += (counts(c) * math.exp(child(c) + costs(c) - partition))
          }
          c += 1;
        }
        new SufficientStatistics(suffStats);
      }

      lazy val partition = childProjection.partition;

      def edgeMarginal(parent: Belief, child: Belief) = this.copy(child = Some(child));

      lazy val childProjection = {
        val cc = new Belief(new DenseVector(Array.tabulate(wordIndex.size)(costs)));
        child.foldLeft(cc)(_ * _);
      }

      def parentProjection = {
        new Belief(allOnes + child.get.partition value);
      }
    }

  }
}

