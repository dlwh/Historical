package dlwh.parsim

import dlwh.cognates._
import dlwh.editdistance._

import breeze.stats.distributions.{SufficientStatistic=>BaseSufficientStatistic}
import breeze.linalg._
import breeze.util.{Encoder, Index}
import java.util.Arrays
import breeze.numerics
import dlwh.util.Lazy

/**
 * 
 * @author dlwh
 */
class WordFactorsFactory(val editDistance:EditDistance, beamThreshold: Double = -10) extends FactorsFactory { factory =>
  import editDistance._

  def max(arr: Array[Double]) = arr.max
  def _max(arr: Array[Double]) = arr.max

  type EdgeParameters = editDistance.Parameters
  case class SufficientStatistic(inner: editDistance.SufficientStatistic) extends BaseSufficientStatistic[SufficientStatistic] {
    def +(stats: SufficientStatistic) = SufficientStatistic(inner + stats.inner)
    def *(weight: Double):SufficientStatistic = {
      SufficientStatistic(inner * weight)
    }
  }


  def initialParameters: EdgeParameters = editDistance.initialParameters
  def emptySufficientStatistic = new SufficientStatistic(editDistance.emptySufficientStatistic)

  private type edSS = editDistance.SufficientStatistic

  def factorsFor[T](legalWords: Set[Word], edgeParameters: Map[T,EdgeParameters]) = {
    new Factors(legalWords,edgeParameters, true)
  }



  def optimize[T](tree: Tree[T], suffStats: Map[T, SufficientStatistic]) = {
    editDistance.makeParameters(suffStats.mapValues(_.inner))
  }


  class Factors[Language](legalWords: Set[Word], costMatrix: (Language)=>Parameters, viterbi:Boolean) extends dlwh.parsim.Factors[Language] {
    type SufficientStatistic = factory.SufficientStatistic

    val wordIndex = Index(legalWords)
    def uniformBelief = new Belief(allOnes,0.0)

    def indicatorBelief(w: Word) = new Belief({
      val r = Encoder.fromIndex(wordIndex).mkDenseVector(Double.NegativeInfinity)
      r(wordIndex(w)) = 0.0
      r
    }, 0.0)
    val allOnes = DenseVector.zeros[Double](wordIndex.size)

    private var _rootMessage : Belief = null;
    def rootMessage(t: Language) = synchronized {
      if(_rootMessage == null) {
        val matrix = costMatrix(t)
        val v = Encoder.fromIndex(wordIndex).tabulateDenseVector(editDistance.distance(matrix,"",_))
        _rootMessage = new Belief(v,v.max)
      }

      _rootMessage
    }

    def logNormalizeInPlace(v: DenseVector[Double]) = {
      v -= softmax(v)
      v
    }

    import collection.{mutable=>m}
    private val precomputedCosts = new m.HashMap[Language,ArrayCache] with m.SynchronizedMap[Language,ArrayCache]
    private val precomputedECounts = new m.HashMap[Language,(Int,Int)=>Lazy[edSS]] with m.SynchronizedMap[Language,(Int,Int)=>Lazy[edSS]]

    def edgeFor(child: Language) = {
      val matrix = precomputedCosts.getOrElseUpdate(child,computeCosts(costMatrix(child)))
      def expCosts(wordA: Int, wordB: Int)  = precomputedECounts.getOrElseUpdate(child,computeECounts(costMatrix(child)))(wordA,wordB)
      new Edge(matrix,expCosts _, None,None)
    }

    private def computeCosts(matrix: Parameters): ArrayCache = {
      val result = new ArrayCache(wordIndex.size, wordIndex.size)({ (i:Int,j:Int) =>
        editDistance.distance(matrix,wordIndex.get(i),wordIndex.get(j))
      })
      result
    }

    private def computeECounts(matrix: editDistance.Parameters): (Int, Int) => Lazy[edSS] = {
      val expectedCountsForWords: Array[Array[Lazy[edSS]]] = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        Lazy.delay(editDistance.sufficientStatistics(matrix, wordIndex.get(p),wordIndex.get(c)))
      }
      def foo(a: Int, b:Int) = expectedCountsForWords(a)(b)

      foo
    }

    case class Belief(beliefs: DenseVector[Double], max: Double) extends BaseBelief {
      lazy val partition = { val r = softmax(beliefs); assert(!r.isNaN & !r.isInfinite); r}
      def apply(word: String)= beliefs(wordIndex(word))

      def /(b: Belief):Belief = {
        val diff = beliefs -b.beliefs
        val newMax = _max(diff.data)
        new Belief(diff,newMax)
      }

      def *(b: Belief):Belief = {
        val newBeliefs = beliefs + b.beliefs
        val newMax = _max(newBeliefs.data)
        val r = new Belief(newBeliefs, newMax)
        r
      }

      override def toString() = ("Belief: " + Encoder.fromIndex(wordIndex).decode(beliefs))

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score)
        new Belief(scaled, max + score)
      }

    }

    type EdgeMarginal = Edge

    class Edge(costs: ArrayCache, baseCounts: (Int,Int)=>Lazy[edSS],
               val parent: Option[Belief]=None,
               val child: Option[Belief]=None) extends BaseEdge with BaseEdgeMarginal {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(costs,baseCounts,Some(parent),Some(child))
      }

      def score(a: Word, b: Word) = {
        costs(wordIndex(a),wordIndex(b))
      }

      def sufficientStatistic = {
        val parent = this.parent.get.beliefs
        val child = this.child.get.beliefs
        val parentMax = this.parent.get.max
        val childMax = this.child.get.max
        var p = 0
        var result = editDistance.emptySufficientStatistic
        while(p < parent.size) {
          var c = 0
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = math.exp(parent(p) + child(c) + costs(p,c) - partition)
                if(score > 1E-5)
                  result += (baseCounts(p,c).result * score)
              }
              c += 1
            }

          p += 1
        }

        SufficientStatistic(result)

      }

      lazy val partition = {
        val parent = this.parent.get.beliefs
        val child = this.child.get.beliefs
        val scores = negativeInfinityArray(parent.size * child.size)
        val parentMax = this.parent.get.max
        val childMax = this.child.get.max
        var p = 0
        var i = 0
        while(p < parent.size) {
          var c = 0
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + costs(p,c)
                scores(i) = score
                i += 1
              }
              c += 1
            }

          p += 1
        }
        numerics.logSum(scores, i)
      }



      def parentProjection:Belief = { // just a matrix multiply in log space.
        val newParent = DenseVector.zeros[Double](wordIndex.size)
        val childBeliefs = this.child.get.beliefs
        val scores: Array[Double] = negativeInfinityArray(childBeliefs.size)
        for( parent <- 0 until wordIndex.size) {
          var child = 0
          while(child < wordIndex.size) {
            if(childBeliefs(child) != Double.NegativeInfinity)
              scores(child) = childBeliefs(child) + costs(parent,child)
            child += 1
          }
          newParent(parent) = if(viterbi) max(scores) else numerics.logSum(scores)
          assert(!newParent(parent).isNaN && !newParent(parent).isInfinite)
        }
        val result = parent.foldLeft(new Belief(newParent, max(newParent.data)))(_ * _)
        result
      }

      def childProjection:Belief = {
        val newChild = DenseVector.zeros[Double](wordIndex.size)
        val parentBeliefs = this.parent.get.beliefs
        val scores = negativeInfinityArray(parentBeliefs.size)
        for( child <- 0 until wordIndex.size) {
          var parent = 0
          while(parent < wordIndex.size) {
            if(parentBeliefs(parent) != Double.NegativeInfinity)
              scores(parent) = parentBeliefs(parent) + costs(parent,child)
            parent += 1
          }
          newChild(child) = if(viterbi) max(scores) else numerics.logSum(scores)
          assert(!newChild(child).isNaN && !newChild(child).isInfinite)
        }
        val result = child.foldLeft(new Belief(newChild, (newChild.data).max))(_ * _)
        result
      }
    }
  }

  def negativeInfinityArray(size: Int): Array[Double] = {
    val r = new Array[Double](size)
    Arrays.fill(r,Double.NegativeInfinity)
    r
  }
}