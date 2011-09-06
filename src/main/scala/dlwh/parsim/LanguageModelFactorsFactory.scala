package dlwh.parsim

import dlwh.cognates._
import dlwh.editdistance._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalanlp.util.{Encoder, Index, Lazy}
import scalala.library.{Library, Numerics}

/**
 * 
 * @author dlwh
 */

class LanguageModelFactorsFactory(charIndex: Index[Char]) extends FactorsFactory { factory =>
  val pe = new AlignmentPairEncoder(charIndex)

  def factorsFor[T](legalWords: Set[Word], edgeParameters: (T) => EdgeParameters) = new Factors(Index(legalWords),edgeParameters)

  def optimize[T](suffStats: Map[T, SufficientStatistic]):Map[T,EdgeParameters] = {
    val allCounts = suffStats.values.foldLeft(DenseVector.zeros[Double](charIndex.size * charIndex.size)) { (v,v2) => v += v2.transitions; v}
    suffStats.mapValues { stats =>
      val totals = new Array[Double](charIndex.size)
      val counts = allCounts * .5 + stats.transitions
      for( (k,v) <- counts.pairsIteratorNonZero) {
        val (prev,_) = pe.decode(k)
        totals(prev) += v
      }

      new EdgeParameters {
        def apply(prev: Char, next: Char) = {
          val r = math.log(counts(pe.encode(prev,next)) + 0.01) - math.log(totals(charIndex(prev)) + .01 * charIndex.size)
          assert(!r.isNaN, counts(pe.encode(prev,next)) -> (totals(charIndex(prev)) + .1 * charIndex.size))
          r
        }
      }

    }
  }


  lazy val emptySufficientStatistics = new SufficientStatistic(SparseVector.zeros[Double](charIndex.size * charIndex.size))

  val initialParameters :EdgeParameters=  new EdgeParameters {
    val theScore = -math.log(charIndex.size)
    def apply(prev: Char, next: Char) =  theScore
  }

  trait EdgeParameters {
    def apply(prev: Char, next: Char):Double
  }
  case class SufficientStatistic(transitions: Vector[Double]) extends scalanlp.stats.distributions.SufficientStatistic[SufficientStatistic] {
    def +(stats: SufficientStatistic) = SufficientStatistic(this.transitions + stats.transitions)

    def *(weight: Double) = SufficientStatistic(this.transitions * weight)
  }

  class Factors[Language](legalWords: Index[Word], parameters: Language=>EdgeParameters) extends dlwh.parsim.Factors[Language] {
    val wordIndex = Index(legalWords)
    def initialBelief(lang: Language) = new Belief(allOnes)

    def initialMessage(from: Language, to: Language) = new Belief(allOnes)

    def beliefForWord(w: Word) = new Belief({
      val r = Encoder.fromIndex(wordIndex).mkDenseVector(Double.NegativeInfinity)
      r(wordIndex(w)) = 0.0
      r
    })
    val allOnes = DenseVector.zeros[Double](wordIndex.size)

    val rootMessage = {
      val allOnes = DenseVector.zeros[Double](wordIndex.size)
      new Belief(logNormalizeInPlace(allOnes))
    }

    def logNormalizeInPlace(v: DenseVector[Double]) = {
      v -= Library.softmax(v)
      v
    }

    import collection.{mutable=>m}
    private val precomputedECounts = new m.HashMap[Language,(Int)=>Lazy[SparseVector[Double]]] with m.SynchronizedMap[Language,(Int)=>Lazy[SparseVector[Double]]]
    private val precomputedCosts = new m.HashMap[Language,(Int)=>Lazy[Double]] with m.SynchronizedMap[Language,(Int)=>Lazy[Double]]

    def edgeFor(parent: Language, child: Language) = {
      val counts = precomputedECounts.getOrElseUpdate(child,computeECounts(parameters(child))) andThen (_.result)
      val scores = precomputedCosts.getOrElseUpdate(child,computeCosts(parameters(child))) andThen (_.result)
      new Edge(scores, counts)
    }

    private def computeCosts(parameters: EdgeParameters) = {
      Array.tabulate(wordIndex.size) { c =>
        Lazy.delay {
          val w = wordIndex.get(c)
          var cost = 0.0
          var ch = '\0'
          var i = 0
          while(i < w.length) {
            val chn = w(i)
            cost += parameters(ch,chn)
            ch = chn
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
          val w = wordIndex.get(c)
          val result = SparseVector.zeros[Double](charIndex.size * charIndex.size)
          var ch = '\0'
          var i = 0
          while(i < w.length) {
            val chn = w(i)
            result(pe.encode(ch,chn)) += 1
            ch = chn
            i += 1
          }
          result(pe.encode(ch,'\0')) += 1
          result
        }
      }
    }

    case class Belief(beliefs: DenseVector[Double]) extends BaseBelief {
      lazy val partition = { val r = Numerics.logSum(beliefs.data); assert(!r.isNaN & !r.isInfinite); r}
      def apply(word: String)= beliefs(wordIndex(word))

      def /(b: Belief):Belief = {
        val diff = beliefs -b.beliefs
        new Belief(diff)
      }

      def *(b: Belief):Belief = {
        val r = new Belief(beliefs + b.beliefs)
        r
      }

      override def toString() = ("Belief: " + Encoder.fromIndex(wordIndex).decode(beliefs))

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score)
        new Belief(scaled)
      }

    }

    case class Edge(costs: Int=>Double, counts: Int=>SparseVector[Double], child: Option[Belief]=None) extends BaseEdge with BaseEdgeMarginal {
      def score(a: Word, b: Word) = costs(wordIndex(b))

      def sufficientStatistics = {
        val child = this.child.get.beliefs
        var c = 0
        val suffStats = SparseVector.zeros[Double](charIndex.size * charIndex.size)
        while(c < child.size) {
          if(child(c) != Double.NegativeInfinity) {
            suffStats += (counts(c) * math.exp(child(c) + costs(c) - partition))
          }
          c += 1
        }
        SufficientStatistic(suffStats)
      }

      lazy val partition = childProjection.partition

      def edgeMarginal(parent: Belief, child: Belief) = this.copy(child = Some(child))

      lazy val childProjection = {
        val cc = new Belief(DenseVector.tabulate(wordIndex.size)(costs))
        child.foldLeft(cc)(_ * _)
      }

      def parentProjection = {
        new Belief(allOnes + child.get.partition)
      }
    }

  }
}
