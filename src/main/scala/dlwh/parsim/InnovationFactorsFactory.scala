package dlwh.parsim

import dlwh.cognates._

import scalala.tensor.sparse.SparseVector
import scalanlp.util.Index
import scalala.library.Numerics
import scalanlp.util._
import scalala.tensor.mutable.Vector
import scalanlp.tensor.sparse.OldSparseVector
import scalala.library.Library._
import java.util.Arrays
import scalala.tensor.dense.DenseVector
import scalanlp.stats.distributions.{SufficientStatistic=>BaseSufficientStatistic}
import scalanlp.optimize.{DiffFunction, FirstOrderMinimizer}
import scalanlp.maxent.{EasyMaxEnt, MaxEntObjectiveFunction}
import scalala.tensor.{Counter2, Counter}
import phylo.Tree

/**
 * 
 * @author dlwh
 */
class InnovationFactorsFactory[F<:FactorsFactory,
                              IF<:FactorsFactory](val baseFactory: F,
                                                  val innovationFactory: IF,
                                                  val initialInnovationProb: Double, useBranchLengths: Boolean = false,
                                                  beamThreshold: Double = - 10) extends FactorsFactory { outer =>

  val viterbi = true


  case class EdgeParameters(params: baseFactory.EdgeParameters,
                            innovParams: innovationFactory.EdgeParameters,
                            innovationProb: Double)

  case class SufficientStatistic(alignmentCounts: baseFactory.SufficientStatistic,
                                  innovationCounts: innovationFactory.SufficientStatistic,
                                  probInnovation: Double,
                                  n: Double) extends BaseSufficientStatistic[SufficientStatistic] {
    def +(that: SufficientStatistic) = {
      SufficientStatistic(alignmentCounts + that.alignmentCounts,
        innovationCounts + that.innovationCounts,
        probInnovation + that.probInnovation,
        n + that.n)
    }
    def *(weight: Double) = {
      SufficientStatistic(alignmentCounts * weight, innovationCounts * weight, probInnovation * weight, n * weight)
    }
  }

  def factorsFor[T](legalWords: Set[Word], edgeParameters: Map[T,EdgeParameters]) =  {
    val factors = baseFactory.factorsFor(legalWords, edgeParameters.mapValues(_.params).withDefault(k => edgeParameters(k).params))
    val innovFactors = innovationFactory.factorsFor(legalWords, edgeParameters.mapValues(_.innovParams).withDefault(k => edgeParameters(k).innovParams))
    new Factors(Index(legalWords),factors, innovFactors, edgeParameters)
  }

  def optimize[T](tree: Tree[T], stats: Map[T,SufficientStatistic]) = {
    val newInnerParams = baseFactory.optimize(tree, stats.mapValues(_.alignmentCounts))
    val newInnovParams = innovationFactory.optimize(tree, stats.mapValues(_.innovationCounts))
    val innoProbs = optimizeInnovation(tree, stats.mapValues(s => (s.probInnovation, s.n)))
    println(innoProbs)
    newInnerParams.map{case (k,inner) => k -> EdgeParameters(inner,newInnovParams(k), innoProbs(k))}.withDefaultValue(initialParameters)
  }

  var iter = 0

  def optimizeInnovation[Language](tree: Tree[Language], stats: Map[Language,(Double,Double)]) = {
    val scores = tree.postorder.map(x => x.label -> x.branchLength).toMap
    iter += 1
    if(useBranchLengths && iter > 10) {
      val obj = new GlobalRateObjective[Language](scores, stats)
      val opt = FirstOrderMinimizer.OptParams(regularization = 1E-4, useStochastic = false, maxIterations = 50)
      val result = opt.minimizer(obj).minimize(obj, obj.enc.mkDenseVector())
      obj.extractProbs(result)
    } else {
      val obj = new InnovationObjective[Language](stats)
      val opt = FirstOrderMinimizer.OptParams(regularization = 1E-4, useStochastic = false, maxIterations = 50)
      val result = opt.minimizer(obj).minimize(obj, obj.enc.mkDenseVector())
      obj.extractProbs(result)
    }
  }

  def initialParameters = new EdgeParameters(baseFactory.initialParameters,
    innovationFactory.initialParameters,
    initialInnovationProb)
  def emptySufficientStatistic = SufficientStatistic(baseFactory.emptySufficientStatistic,innovationFactory.emptySufficientStatistic,0,0)

  case class Factors[T](wordIndex: Index[Word],
                        baseFactors: baseFactory.Factors[T],
                        innovationFactors: innovationFactory.Factors[T],
                        params: T=>EdgeParameters) extends dlwh.parsim.Factors[T] {
    type SufficientStatistic = outer.SufficientStatistic


    def uniformBelief = NullState

    def indicatorBelief(w: Word) = {
      val r = new OldSparseVector(wordIndex.size, Double.NegativeInfinity)
      r(wordIndex(w)) = 0
      new SingleState(r)
    }

    def rootMessage(t: T) = {
      val innovRoot = innovationFactors.rootMessage(t)
      val r = Encoder.fromIndex(wordIndex).tabulateDenseVector(innovRoot.apply _)
      SingleState(r)
    }

    def edgeFor(parent: T) = {
      val edge = baseFactors.edgeFor(parent)
      val innovEdge = innovationFactors.edgeFor(parent)
      val matrix = precomputedCosts.getOrElseUpdate(parent,computeCosts(edge, innovEdge, params(parent).innovationProb))
      def expCounts(wordA: Int, wordB: Int)  = {
        precomputedECounts.getOrElseUpdate(parent, computeECounts(edge))(wordA, wordB)
      }
      def innovExpCounts(wordA: Int, wordB: Int)  = {
        precomputedIECounts.getOrElseUpdate(parent, computeIECounts(innovEdge))(wordA, wordB)
      }
      new Edge(matrix, expCounts _, innovExpCounts _)
    }

    sealed trait Belief extends BaseBelief {
      val beliefs : Vector[Double]
      lazy val max = beliefs.max
      lazy val partition = softmax(beliefs)
    }

    case object NullState extends Belief {
      def apply(w: Word) = 0.0
      val beliefs = SparseVector.zeros[Double](wordIndex.size)

      def /(b: Belief) = b match {
        case SingleState(b) => SingleState(- b)
        case NullState => this

      }

      def *(b: Belief) = b

      def scaleBy(score: Double) = TODO

      override def normalized = this

    }
    case class SingleState(beliefs: Vector[Double]) extends Belief { // no homoplasy
      def apply(w: Word) = beliefs(wordIndex(w))

      assert(beliefs.size == wordIndex.size)
      def /(b: Belief) = b match {
        case SingleState(b) => SingleState(beliefs - b)
        case NullState => this
      }

      def *(b: Belief) = b match {
        case SingleState(b) => SingleState(beliefs + b)
        case NullState => this
      }

      def scaleBy(score: Double) = {
        new SingleState(beliefs + score)
      }

      override def toString() = {
        val ctr = (Encoder.fromIndex(wordIndex).decode(beliefs))
        "SingleState: " + ctr
      }

    }

    sealed trait EdgeMarginal extends BaseEdgeMarginal {
      def posteriorConservativeProb: Double = (1 - posteriorInnovationProb)
      def posteriorInnovationProb: Double

      override def toString = (
        getClass.getSimpleName+"(conP=" + posteriorConservativeProb
          + ", inno=" + posteriorInnovationProb +")"
      )
    }

    case class Edge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistic],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistic]) extends BaseEdge {
      def posteriorInnovationProb = 0.0

      def posteriorFromInnovativeParent = 0.0

      def edgeMarginal(parent: Belief, child: Belief):EdgeMarginal = (parent,child) match {
        case (NullState,NullState) => sys.error("...")
        case (NullState,x:SingleState) =>
          SingleSingleEdge(edgeParams,baseCounts,innovBaseCounts,None,Some(x))
        case (y:SingleState,NullState) =>
          SingleSingleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),None)
        case (y:SingleState,x: SingleState) =>
          SingleSingleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),Some(x))
        case _ => sys.error("bad edge types!" + this + " " + parent + " " + child)
      }

      def childProjection:Belief = XXX
      def parentProjection:Belief = XXX

      def sufficientStatistic:SufficientStatistic = XXX

      def partition:Double = XXX

      def score(parent: Word, child: Word):Double = XXX
    }

    case class SingleSingleEdge(edgeParams: EdgeParams,
                                baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistic],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistic],
                                parent: Option[SingleState] = None,
                                child: Option[SingleState] = None) extends EdgeMarginal {
      def edgeMarginal(parent: Belief, child: Belief):EdgeMarginal = (parent,child) match {
        case (NullState,NullState) => copy(parent=None,child=None)
        case (NullState,x:SingleState) => copy(parent=None,child=Some(x))
        case (y:SingleState,x:SingleState) => copy(parent=Some(y),child=Some(x))
        case (y:SingleState,NullState) => copy(parent=Some(y),child=None)
        case _ => sys.error("bad edge types!" + this + " " + parent + " " + child)
      }

      def score(a: Word, b: Word) = {
        edgeParams.summed(wordIndex(a),wordIndex(b))
      }

      def sufficientStatistic: SufficientStatistic = {
        val parent = this.parent.get.beliefs
        val parentMax = this.parent.get.max
        val child = this.child.get.beliefs
        val childMax = this.child.get.max
        var p = 0

        var wordChangeCounts = baseFactory.emptySufficientStatistic
        var innovCounts = innovationFactory.emptySufficientStatistic
        var pInnov = 0.0
        while(p < parent.size) {
          var c = 0
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + edgeParams.summed(p,c) - partition)
                // log p(new|wc,wp) (numerator)
                val posteriorInnovation = edgeParams.withInnovation(p, c) + edgeParams.logInnov
                // log p(not new|wc,wp) (numerator)
                val nonInnovation = edgeParams.withoutInnovation(p,c) + edgeParams.logNonInnov
                // p(new|wc,wp) (normalized)
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation))
                val normNonInnov = 1-normalizedPosteriorInnovation
                if(normNonInnov * score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(p,c).result * (normNonInnov * score))
                if(normalizedPosteriorInnovation * score > 1E-8) // counts for p and c weighted by p(new,wc,wp)
                  innovCounts += (innovBaseCounts(p,c).result * (normalizedPosteriorInnovation * score))


                pInnov += normalizedPosteriorInnovation * score
              }

              c += 1
            }

          p += 1
        }

        SufficientStatistic(wordChangeCounts,innovCounts, pInnov,1)
      }


      lazy val posteriorInnovationProb = {
        val parent = this.parent.get.beliefs
        val child = this.child.get.beliefs
        val parentMax = this.parent.get.max
        val childMax = this.child.get.max
        var p = 0

        var pInnov = 0.0
        while(p < parent.size) {
          var c = 0
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + edgeParams.summed(p,c) - partition)
                // log p(new|wc,wp) (numerator)
                val posteriorInnovation = edgeParams.withInnovation(p, c) + edgeParams.logInnov
                // log p(not new|wc,wp) (numerator)
                val nonInnovation = edgeParams.withoutInnovation(p,c) + edgeParams.logNonInnov
                // p(new|wc,wp) (normalized)
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation))

                pInnov += normalizedPosteriorInnovation * score
              }

              c += 1
            }

          p += 1
        }

        pInnov
      }

      lazy val partition = {
        val parent = this.parent.get.beliefs
        val child = this.child.get.beliefs
        val scores = negativeInfinityArray(parent.size * child.size)
        var p = 0
        var i = 0
        val parentMax = this.parent.get.max
        val childMax = this.child.get.max
        while(p < parent.size) {
          var c = 0
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + edgeParams.summed(p,c)
                scores(i) = score
              }
              i += 1
              c += 1
            }

          p += 1
        }
        Numerics.logSum(scores, i)
      }



      def parentProjection:Belief = { // just a matrix multiply in log space.
        val newParent = DenseVector.zeros[Double](wordIndex.size)
        val childBeliefs = this.child.get.beliefs
        val maxChild = this.child.get.max
        var parent = 0
        while(parent < newParent.size) {
          val scores = negativeInfinityArray(wordIndex.size)
          var child = 0
          while(child < childBeliefs.size) {
            if(childBeliefs(child) >= maxChild + beamThreshold || parent == child)
              scores(child) = childBeliefs(child) + edgeParams.summed(parent,child)
            child += 1
          }
          newParent(parent) = if(viterbi) (scores).max else Numerics.logSum(scores)
          parent += 1
        }
        val result = this.parent.foldLeft(new SingleState(newParent))( (a,b) => (a * b).asInstanceOf[SingleState])
        result
      }

      def childProjection:Belief = {
        val newChild = DenseVector.zeros[Double](wordIndex.size)
        val parentBeliefs = this.parent.get.beliefs
        val maxParent = this.parent.get.max
        for( child <- 0 until newChild.size) {
          val scores = negativeInfinityArray(wordIndex.size)
          var parent = 0
          while(parent < parentBeliefs.size) {
            if(parentBeliefs(parent) >= maxParent + beamThreshold || parent == child)
              scores(parent) = parentBeliefs(parent) + edgeParams.summed(parent,child)
            parent += 1
          }
          newChild(child) = if(viterbi) scores.max else Numerics.logSum(scores)
        }
        val result = child.foldLeft(new SingleState(newChild))( (a,b) => (a * b).asInstanceOf[SingleState])
        result
      }
    }
    case class EdgeParams(summed: ArrayCache,
                          withoutInnovation: ArrayCache,
                          withInnovation: ArrayCache,
                          logInnov: Double, logNonInnov: Double)

    import collection.{mutable=>m}
    private val precomputedCosts = new m.HashMap[T,EdgeParams] with m.SynchronizedMap[T,EdgeParams]
    private val precomputedECounts = new m.HashMap[T,(Int,Int)=>Lazy[baseFactory.SufficientStatistic]] with m.SynchronizedMap[T,(Int,Int)=>Lazy[baseFactory.SufficientStatistic]]
    private val precomputedIECounts = new m.HashMap[T,(Int,Int)=>Lazy[innovationFactory.SufficientStatistic]] with m.SynchronizedMap[T,(Int,Int)=>Lazy[innovationFactory.SufficientStatistic]]
    private def computeCosts(edge: baseFactors.Edge, innovEdge: innovationFactors.Edge, innovationProb: Double) = {
      val withoutInnovation = new ArrayCache(wordIndex.size,wordIndex.size)( {(p,c) =>
        edge.score(wordIndex.get(p),wordIndex.get(c))
      })
      val logProbInnovation = math.log(innovationProb)
      val nonInnovation = math.log(1-innovationProb)

      val withInnovation = new ArrayCache(wordIndex.size,wordIndex.size)( {(p,c) =>
        innovEdge.score(wordIndex.get(p),wordIndex.get(c))
      })

      val summed = new ArrayCache(wordIndex.size,wordIndex.size) ({ (p,c) =>
        if(viterbi) Numerics.logSum(withoutInnovation(p,c) + nonInnovation,withInnovation(p, c) + logProbInnovation)
        else math.max(withoutInnovation(p,c) + nonInnovation,withInnovation(p, c) + logProbInnovation)
      })

      EdgeParams(summed,withoutInnovation,withInnovation, logProbInnovation, nonInnovation)
    }

    private def computeECounts(edge: baseFactors.Edge): ((Int, Int) => Lazy[baseFactory.SufficientStatistic]) = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        lazy val marg = edge.edgeMarginal(baseFactors.indicatorBelief(wordIndex.get(p)),baseFactors.indicatorBelief(wordIndex.get(c)))
        Lazy.delay(marg.sufficientStatistic)
      }
      def foo(a:Int,b:Int) = expectedCountsForWords(a)(b)

      foo _
    }

    private def computeIECounts(edge: innovationFactors.Edge): ((Int, Int) => Lazy[innovationFactory.SufficientStatistic]) = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        lazy val marg = edge.edgeMarginal(innovationFactors.indicatorBelief(wordIndex.get(p)),innovationFactors.indicatorBelief(wordIndex.get(c)))
        Lazy.delay(marg.sufficientStatistic)
      }
      def foo(a:Int,b:Int) = expectedCountsForWords(a)(b)

      foo _
    }
  }

  def negativeInfinityArray(size: Int): Array[Double] = {
    val r = new Array[Double](size)
    Arrays.fill(r,Double.NegativeInfinity)
    r
  }

}

class GlobalRateObjective[T](branchLengths: Map[T,Double], innovationCounts: Map[T,(Double,Double)]) extends DiffFunction[DenseVector[Double]] {
  val langIndex = Index(innovationCounts.keys)
  val enc = Encoder.fromIndex(langIndex)
  val encodedYes = enc.encodeDense(Counter(innovationCounts.map { case (k,v) => k -> v._1}))
  val encodedTotal = enc.encodeDense(Counter(innovationCounts.map { case (k,v) => k -> v._2}))
  val encodedBranchLengths = enc.encodeDense(Counter(branchLengths))
  def initial = DenseVector(1E-4)
  def calculate(x: DenseVector[Double]) = {
    var ll = 0.0
    val grad = DenseVector.zeros[Double](x.size)
    println(x)

    var lang = 0
    while(lang < encodedYes.size) {
      val branchLength = encodedBranchLengths(lang)
      if(!branchLength.isInfinite) {
        assert(encodedYes(lang) <= encodedTotal(lang), encodedYes(lang) + " " + encodedTotal(lang))
        val pYes = Numerics.sigmoid(branchLength * x(0))
        ll -= (math.log(pYes) * encodedYes(lang) + math.log(1-pYes) * (encodedTotal(lang) - encodedYes(lang)))
        val yesGradPart = encodedYes(lang)
        val noGradPart = -(encodedTotal(lang) - encodedYes(lang)) * pYes
        val gradContribution = yesGradPart + noGradPart
        grad(0) -= gradContribution * (branchLength)
      }
      lang += 1
    }

    (ll,grad)
  }

  def extractProbs(x: DenseVector[Double]): Map[T, Double] = {
    branchLengths.mapValues(l => Numerics.sigmoid(l* x(0)))
  }

}

class InnovationObjective[T](innovationCounts: Map[T,(Double,Double)]) extends DiffFunction[DenseVector[Double]] {
  val features = Index(innovationCounts.keysIterator.map(Some.apply(_)) ++ Iterator.single(None))
  val enc = Encoder.fromIndex(features)
  val globalFeature = features(None)
  assert(globalFeature != -1)
  val encodedYes = enc.encodeDense(Counter(innovationCounts.map { case (k,v) => Some(k) -> v._1}))
  val encodedTotal = enc.encodeDense(Counter(innovationCounts.map { case (k,v) => Some(k) -> v._2}))
  def calculate(x: DenseVector[Double]) = {
    var ll = 0.0
    val grad = DenseVector.zeros[Double](x.size)

    var lang = 0
    while(lang < encodedYes.size) {
      assert(encodedYes(lang) <= encodedTotal(lang), encodedYes(lang) + " " + encodedTotal(lang))
      if(lang != globalFeature) {
        val pYes = Numerics.sigmoid(x(lang) + x(globalFeature))
        ll -= (math.log(pYes) * encodedYes(lang) + math.log(1-pYes) * (encodedTotal(lang) - encodedYes(lang)))
        val yesGradPart = encodedYes(lang) * (1 - pYes)
        val noGradPart = -(encodedTotal(lang) - encodedYes(lang)) * pYes
        val gradContribution = yesGradPart + noGradPart
        grad(lang) -= gradContribution
        grad(globalFeature) -= gradContribution
      }
      lang += 1
    }

    (ll,grad)
  }

  def extractProbs(x: DenseVector[Double]): Map[T, Double] = {
    val rawArray =Array.tabulate(innovationCounts.size) { lang =>  Numerics.sigmoid(x(lang) + x(globalFeature))}
    enc.decode(rawArray).map { case (k,v) => (k.get,v)}
  }

}