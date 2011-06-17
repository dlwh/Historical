package dlwh.newcognates

import scalanlp.fst.fast.AutomatonFactory
import scalala.Scalala.{iFigure => _, _scalala_figure => _, _}
import scalanlp.math.Numerics
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.tensor.Vector;
import scalala.tensor.sparse.SparseVector
import scalanlp.util.{Lazy, Encoder, Index}
import java.util.Arrays

/**
 * 
 * @author dlwh
 */
class WordFactorsFactory(val editDistance:EditDistance, beamThreshold: Double = -10) extends SuffStatsFactorsFactory {
  import editDistance._;

  type EdgeParameters = editDistance.Parameters;
  case class SufficientStatistics(inner: editDistance.SufficientStatistics) extends BaseSufficientStatistics {
    def +(stats: SufficientStatistics) = SufficientStatistics(inner + stats.inner);
    def *(weight: Double):SufficientStatistics = {
      SufficientStatistics(inner * weight);
    }
  }


  def initialParameters: EdgeParameters = editDistance.initialParameters;
  def emptySufficientStatistics = new SufficientStatistics(editDistance.emptySufficientStatistics);

  private type edSS = editDistance.SufficientStatistics;

  type Factors = WordFactors
  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters) = {
    new WordFactors(legalWords,edgeParameters, true);
  }


  def optimize(suffStats: Map[Language, SufficientStatistics]) = {
    editDistance.makeParameters(suffStats.mapValues(_.inner));
  }


  class WordFactors(legalWords: Set[Word], costMatrix: (Language)=>Parameters, viterbi:Boolean) extends FactorsWithSuffStats {

    val wordIndex = Index(legalWords);
    def initialBelief(lang: Language) = new Belief(allOnes,0.0);

    def initialMessage(from: Language, to: Language) = new Belief(allOnes, 0.0);

    def beliefForWord(w: Word) = new Belief({
      val r = Encoder.fromIndex(wordIndex).mkDenseVector(Double.NegativeInfinity);
      r(wordIndex(w)) = 0.0;
      r
    }, 0.0)
    val allOnes = new DenseVector(wordIndex.size);

    val rootMessage = {
      val allOnes = logNormalizeInPlace(new DenseVector(wordIndex.size));
      new Belief(allOnes,allOnes(0));
    }

    def logNormalizeInPlace(v: DenseVector) = {
      v -= Numerics.logSum(v);
      v
    }

    import collection.{mutable=>m}
    private val precomputedCosts = new m.HashMap[(Language,Language),ArrayCache] with m.SynchronizedMap[(Language,Language),ArrayCache];
    private val precomputedECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[edSS]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[edSS]];

    def edgeFor(parent: Language, child: Language) = {
      val matrix = precomputedCosts.getOrElseUpdate(parent->child,computeCosts(costMatrix(child)));
      def expCosts(wordA: Int, wordB: Int)  = precomputedECounts.getOrElseUpdate(parent->child,computeECounts(costMatrix(child)))(wordA,wordB);
      new Edge(matrix,expCosts _, None,None);
    }

    private def computeCosts(matrix: Parameters): ArrayCache = {
      val result = new ArrayCache(wordIndex.size, wordIndex.size)({ (i:Int,j:Int) =>
        editDistance.distance(matrix,wordIndex.get(i),wordIndex.get(j))
      })
      result;
    }

    private def computeECounts(matrix: editDistance.Parameters): (Int, Int) => Lazy[edSS] = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        Lazy.delay(editDistance.sufficientStatistics(matrix, wordIndex.get(p),wordIndex.get(c)))
      };
      {(a:Int,b:Int) => (expectedCountsForWords(a)(b))}
    }

    case class Belief(beliefs: DenseVector, max: Double) extends BeliefBase {
      lazy val partition = { val r = Numerics.logSum(beliefs); assert(!r.isNaN & !r.isInfinite); r}
      def apply(word: String)= beliefs(wordIndex(word));

      def /(b: Belief):Belief = {
        val diff = beliefs -b.beliefs value;
        val newMax = Numerics.max(diff.data);
        new Belief(diff,newMax);
      }

      def *(b: Belief):Belief = {
        val newBeliefs = beliefs + b.beliefs value
        val newMax = Numerics.max(newBeliefs.data);
        val r = new Belief(newBeliefs, newMax);
        r
      }

      override def toString() = ("Belief: " + Encoder.fromIndex(wordIndex).decode(beliefs));

      def normalized = scaleBy(-partition)

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score value);
        new Belief(scaled, max + score);
      }

    }

    class Edge(costs: ArrayCache, baseCounts: (Int,Int)=>Lazy[edSS],
               val parent: Option[Belief]=None,
               val child: Option[Belief]=None) extends EdgeBase with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(costs,baseCounts,Some(parent),Some(child));
      }

      def score(a: Word, b: Word) = {
        costs(wordIndex(a),wordIndex(b));
      }

      def sufficientStatistics = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        var p = 0;
        var result = editDistance.emptySufficientStatistics;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = math.exp(parent(p) + child(c) + costs(p,c) - partition);
                if(score > 1E-5)
                  result += (baseCounts(p,c).result * score);
              }
              c += 1;
            }

          p += 1
        }

        SufficientStatistics(result);

      }

      lazy val partition = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val scores = negativeInfinityArray(parent.size * child.size);
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        var p = 0;
        var i = 0;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + costs(p,c)
                scores(i) = score;
                i += 1;
              }
              c += 1;
            }

          p += 1
        }
        Numerics.logSum(scores, i);
      }



      def parentProjection:Belief = { // just a matrix multiply in log space.
        val newParent = new DenseVector(wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val scores: Array[Double] = negativeInfinityArray(childBeliefs.size);
        for( parent <- 0 until wordIndex.size) {
          var child = 0;
          while(child < wordIndex.size) {
            if(childBeliefs(child) != Double.NegativeInfinity)
              scores(child) = childBeliefs(child) + costs(parent,child);
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          assert(!newParent(parent).isNaN && !newParent(parent).isInfinite)
        }
        val result = parent.foldLeft(new Belief(newParent, Numerics.max(newParent.data)))(_ * _);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val scores = negativeInfinityArray(parentBeliefs.size);
        for( child <- 0 until wordIndex.size) {
          var parent = 0;
          while(parent < wordIndex.size) {
            if(parentBeliefs(parent) != Double.NegativeInfinity)
              scores(parent) = parentBeliefs(parent) + costs(parent,child);
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          assert(!newChild(child).isNaN && !newChild(child).isInfinite)
        }
        val result = child.foldLeft(new Belief(newChild, Numerics.max(newChild.data)))(_ * _);
        result;
      }
    }
  }

  def negativeInfinityArray(size: Int): Array[Double] = {
    val r = new Array[Double](size);
    Arrays.fill(r,Double.NegativeInfinity);
    r;
  }
}