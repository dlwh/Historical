package dlwh.newcognates

import scalala.tensor.sparse.SparseVector
import scalanlp.util.Index
import scalala.tensor.adaptive.AdaptiveVector
import scalanlp.math.Numerics
import scalanlp.util._;
import scalala.tensor.Vector;
import scalala.Scalala._
import java.util.Arrays
import scalanlp.optimize.FirstOrderMinimizer
import scalala.tensor.dense.DenseVector

/**
 * 
 * @author dlwh
 */
class ContinuumFactorsFactory[F<:SuffStatsFactorsFactory,
                              IF<:SuffStatsFactorsFactory](tree: Tree,
                                                           val baseFactory: F,
                                                           val innovationFactory: IF,
                                                           val initialInnovationProb: Double,
                                                           val initialParentInnovationProb: Double,
                                                           beamThreshold: Double = - 10) extends SuffStatsFactorsFactory {

  val viterbi = true


  case class EdgeParameters(params: baseFactory.EdgeParameters,
                            innovParams: innovationFactory.EdgeParameters,
                            innoParentProb: Double,
                            innovationProb: Double,
                            homoplasyOk: Boolean = false) {
    def enableHomoplasy() = if(homoplasyOk) this else {
      copy(homoplasyOk = true,innoParentProb = innovationProb  * 0.5 / (1-innovationProb * 0.5), innovationProb = innovationProb / 2 );
    }
  }

  case class SufficientStatistics(alignmentCounts: baseFactory.SufficientStatistics,
                                  innovationCounts: innovationFactory.SufficientStatistics,
                                  fromInnovativeParent: Double,
                                  probInnovation: Double,
                                  n: Double, homoplasyOk: Boolean) extends BaseSufficientStatistics {
    def +(that: SufficientStatistics) = {
      SufficientStatistics(alignmentCounts + that.alignmentCounts,
        innovationCounts + that.innovationCounts,
        fromInnovativeParent + that.fromInnovativeParent,
        probInnovation + that.probInnovation,
        n + that.n, that.homoplasyOk);
    }
    def *(weight: Double) = {
      SufficientStatistics(alignmentCounts * weight, innovationCounts * weight, fromInnovativeParent * weight, probInnovation * weight, n * weight, homoplasyOk);
    }
  }

  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters) =  {
    val factors = baseFactory.mkFactors(legalWords, edgeParameters andThen (_.params))
    val innovFactors = innovationFactory.mkFactors(legalWords, edgeParameters andThen (_.innovParams));
    new Factors(Index(legalWords),factors, innovFactors, edgeParameters);
  }

  def optimize(stats: Map[Language,SufficientStatistics]) = {
    val newInnerParams = baseFactory.optimize(stats.mapValues(_.alignmentCounts));
    val newInnovParams = innovationFactory.optimize(stats.mapValues(_.innovationCounts));
    val innoProbs = optimizeInnovation(stats.mapValues(s => (s.probInnovation, s.n)))
    println(innoProbs);
    val innoParentProbs = optimizeInnovation(stats.mapValues(s => (s.fromInnovativeParent, s.n - s.probInnovation)))
    println(innoParentProbs);
    newInnerParams.map{case (k,inner) => k -> EdgeParameters(inner,newInnovParams(k), innoParentProbs(k), innoProbs(k), stats(k).homoplasyOk)}.withDefaultValue(initialParameters);
  }

  def optimizeInnovation(stats: Map[Language,(Double,Double)]) = {
    val obj = new InnovationObjective(stats);
    val opt = FirstOrderMinimizer.OptParams(regularization = 1E-4, useStochastic = false, maxIterations = 50);
    val result = opt.minimizer(obj).minimize(obj, obj.enc.mkDenseVector());
    obj.extractProbs(result);
  }

  def initialParameters = new EdgeParameters(baseFactory.initialParameters,
    innovationFactory.initialParameters,
    initialParentInnovationProb,
    initialInnovationProb);
  def emptySufficientStatistics = SufficientStatistics(baseFactory.emptySufficientStatistics,innovationFactory.emptySufficientStatistics,0,0,0, false);

  case class Factors(wordIndex: Index[Word], baseFactors: baseFactory.Factors, innovationFactors: innovationFactory.Factors, params: Language=>EdgeParameters) extends FactorsWithSuffStats {

    def enableHomoplasyFor(languages: Set[Language]) = {
      def newParams(l: Language) = if(languages(l)) params(l).enableHomoplasy else params(l);
      new Factors(wordIndex, baseFactors, innovationFactors, newParams _)
    }

    def initialBelief(lang: Language) ={ NullState }

    def initialMessage(from: Language, to: Language) = {
      initialBelief(to);
    }

    def beliefForWord(w: Word) = {
      val r = new SparseVector(wordIndex.size);
      r.default = Double.NegativeInfinity
      r(wordIndex(w)) = 0;
      new SingleState(r);
    }

    def rootMessage = if(params(tree.label).homoplasyOk)  {
      val r = new DenseVector(wordIndex.size * wordIndex.size);
      r -= math.log(wordIndex.size);
      DualState(r)
    } else {
      val r = new DenseVector(wordIndex.size);
      r -= math.log(wordIndex.size);
      SingleState(r)
    };
    def edgeFor(parent: Language, child: Language) = {
      val edge = baseFactors.edgeFor(parent, child)
      val innovEdge = innovationFactors.edgeFor(parent,child);
      val matrix = precomputedCosts.getOrElseUpdate(parent->child,computeCosts(edge, innovEdge, params(child).innovationProb, params(child).innoParentProb));
      def expCounts(wordA: Int, wordB: Int)  = {
        precomputedECounts.getOrElseUpdate(parent -> child, computeECounts(edge))(wordA, wordB)
      };
      def innovExpCounts(wordA: Int, wordB: Int)  = {
        precomputedIECounts.getOrElseUpdate(parent -> child, computeIECounts(innovEdge))(wordA, wordB)
      };
      new UnattachedEdge(matrix, expCounts _, innovExpCounts _, params(parent).homoplasyOk, params(child).homoplasyOk)
    }

    sealed trait Belief extends BeliefBase {
      val beliefs : Vector;
      lazy val max = Numerics.max(beliefs);
      lazy val partition = Numerics.logSum(beliefs);
    }
    case class DualState(beliefs: Vector = new AdaptiveVector(wordIndex.size * wordIndex.size)) extends Belief {
      assert(beliefs.size == wordIndex.size * wordIndex.size)
      def apply(w: Word):Double = {
        error("???");
      }
      def /(b: Belief) = b match {
        case DualState(b) => DualState(beliefs - b);
        case SingleState(b) => {
          println("SSS"); TODO
        }
        case NullState => this;
      }

      def *(b: Belief) = b match {
        case DualState(b) => DualState(beliefs + b value);
        case x: SingleState =>TODO
        case NullState => this;
      }

      def scaleBy(score: Double) = {
        new DualState(beliefs + score)
      }

      def normalized = scaleBy(-partition);

      override def toString() = {
        val ctr = (Encoder.fromIndex(new PairIndex(wordIndex,wordIndex)).decode(beliefs));
        "DualState: " + ctr.maxk(2).map(k => k -> ctr(k));
      };


    }

    case object NullState extends Belief {
      def apply(w: Word) = 0.0;
      val beliefs = new AdaptiveVector(wordIndex.size);

      def /(b: Belief) = b match {
        case DualState(b) => DualState(- b);
        case SingleState(b) => SingleState(- b);
        case NullState => this

      }

      def *(b: Belief) = b

      def scaleBy(score: Double) = TODO;

      def normalized = this;

    }
    case class SingleState(beliefs: Vector = new AdaptiveVector(wordIndex.size)) extends Belief { // no homoplasy
      def apply(w: Word) = beliefs(wordIndex(w));

      assert(beliefs.size == wordIndex.size)
      def /(b: Belief) = b match {
        case DualState(b) => println("DDD"); TODO;
        case SingleState(b) => SingleState(beliefs - b);
        case NullState => this;
      }

      def *(b: Belief) = b match {
        case DualState(b) => TODO;
        case SingleState(b) => SingleState(beliefs + b);
        case NullState => this;
      }

      def scaleBy(score: Double) = {
        new SingleState(beliefs + score)
      }

      def normalized = scaleBy(-partition);

      override def toString() = {
        val ctr = (Encoder.fromIndex(wordIndex).decode(beliefs));
        "SingleState: " + ctr.maxk(2).map(k => k -> ctr(k));
      };

    }

    sealed trait Edge extends EdgeBase with HasSufficientStatistics {
      def posteriorConservativeProb: Double = (1 - posteriorInnovationProb - posteriorFromInnovativeParent);
      def posteriorInnovationProb: Double;
      def posteriorFromInnovativeParent:Double;

      override def toString = (
        getClass.getSimpleName+"(conP=" + posteriorConservativeProb
          + ", innP=" + posteriorFromInnovativeParent + ", inno=" + posteriorInnovationProb +")"
      );
    }

    case class UnattachedEdge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistics],
                                parentHomo: Boolean, childHomo: Boolean) extends Edge with HasSufficientStatistics {
      def posteriorInnovationProb = 0.0

      def posteriorFromInnovativeParent = 0.0

      def edgeMarginal(parent: Belief, child: Belief):Edge = (parent,child) match {
        case (NullState,NullState) => this;
        case (NullState,x:DualState) =>
          if(parentHomo) DoubleDoubleEdge(edgeParams,baseCounts,innovBaseCounts,None,Some(x))
          else SingleDoubleEdge(edgeParams,baseCounts,innovBaseCounts,None,Some(x))
        case (NullState,x:SingleState) =>
          if(parentHomo) DoubleSingleEdge(edgeParams,baseCounts,innovBaseCounts,None,Some(x))
          else SingleSingleEdge(edgeParams,baseCounts,innovBaseCounts,None,Some(x))
        case (y:SingleState,NullState) =>
          if(childHomo) SingleDoubleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),None)
          else SingleSingleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),None)
        case (y:SingleState,x:DualState) =>
          SingleDoubleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),Some(x))
        case (y:SingleState,x: SingleState) =>
          SingleSingleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),Some(x))
        case (y:DualState,NullState) =>
          if(childHomo) DoubleDoubleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),None)
          else DoubleSingleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y), None)
        case (y:DualState,x:DualState) =>
          DoubleDoubleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),Some(x))
        case (y:DualState,x: SingleState) =>
          DoubleSingleEdge(edgeParams,baseCounts,innovBaseCounts,Some(y),Some(x))
        case _ => error("bad edge types!" + this + " " + parent + " " + child);
      }

      def childProjection:Belief = XXX
      def parentProjection:Belief = XXX

      def sufficientStatistics:SufficientStatistics = XXX

      def partition:Double = XXX

      def score(parent: Word, child: Word):Double = XXX
    }

    case class SingleDoubleEdge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistics],
                                parent: Option[SingleState] = None, child: Option[DualState]=None) extends Edge with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = (parent,child) match {
        case (NullState,NullState) => copy(parent=None,child=None)
        case (NullState,x:DualState) => copy(parent=None,child=Some(x))
        case (y:SingleState,x:DualState) => copy(parent=Some(y),child=Some(x))
        case (y:SingleState,NullState) => copy(parent=Some(y),child=None)
        case _ => error("bad edge types!" + this + " " + parent + " " + child);
      }

      def posteriorFromInnovativeParent = 0.0

      // not right. whatever.
      def score(a: Word, b: Word) = {
        edgeParams.summed(wordIndex(a),wordIndex(b));
      }

      lazy val partition = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val scores = negativeInfinityArray(parent.size * child.size);
        var p = 0;
        var i = 0;
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + likelihoodTerm(c,p)
                scores(i) = score;
              }
              i += 1;
              c += 1;
            }

          p += 1
        }
        Numerics.logSum(scores, i);
      }

      def posteriorInnovationProb = sufficientStatistics.probInnovation;

      lazy val sufficientStatistics: SufficientStatistics = {
        val parent = this.parent.get.beliefs;
        val parentMax = this.parent.get.max;
        val child = this.child.get.beliefs;
        val childMax = this.child.get.max;
        var p = 0;

        var wordChangeCounts = baseFactory.emptySufficientStatistics;
        var innovCounts = innovationFactory.emptySufficientStatistics;
        var pInnov = 0.0;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + likelihoodTerm(c,p) - partition);
                // log p(new|wc,wp) (numerator)
                val childCon = c / wordIndex.size
                val childInn = c % wordIndex.size
                // log p(new|wc,wp) (numerator)
                val posteriorInnovation = edgeParams.withInnovation(p, childCon) + edgeParams.logInnov;
                // log p(not new|wc,wp) (numerator)
                val nonInnovation = edgeParams.withoutInnovation(p,childCon) + edgeParams.logNonInnov;
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation));
                val normNonInnov = 1-normalizedPosteriorInnovation;
                if(score * normNonInnov > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(p,childCon).result * (score * normNonInnov));
                if(score * normalizedPosteriorInnovation > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  innovCounts += (innovBaseCounts(p,childCon).result * (score  * normalizedPosteriorInnovation));
                if(score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  innovCounts += (innovBaseCounts(p,childInn).result * score);
                pInnov += normalizedPosteriorInnovation * score;
              }

              c += 1;
            }

          p += 1
        }


        SufficientStatistics(wordChangeCounts,innovCounts, 0.0, pInnov,1,true);
      }

      def likelihoodTerm(child: Int, parent:Int): Double = {
        val childCon = child / wordIndex.size
        val childInn = child % wordIndex.size
        val conPart = edgeParams.summed(parent, childCon);
        val innPart = edgeParams.withInnovation(parent, childInn);
        conPart + innPart;
      }

      def parentProjection:Belief = {
        val newParent = new DenseVector(wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val maxChild = this.child.get.max;
        var parent = 0;
        while(parent < newParent.size) {
          val scores = negativeInfinityArray(childBeliefs.size);
          var child = 0;
          while(child < childBeliefs.size) {
            if(childBeliefs(child) >= maxChild + beamThreshold || parent == child)
              // \mu_{child->parent}(w)
               //   = \sum_{wcon,winn} \mu_{wc} (p(w_c|wcon) * p(nonInnov) + p(w_c|winn) * p(innov)) * p(nonInnov) + p(w_c|lm) * p(innov)
              scores(child) = childBeliefs(child) + (
                likelihoodTerm(child, parent)
              )
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          parent += 1;
        }
        val result = this.parent.foldLeft(new SingleState(newParent))( (a,b) => (a * b).asInstanceOf[SingleState]);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val maxParent = this.parent.get.max;
        for( child <- 0 until newChild.size) {
          val scores = negativeInfinityArray(parentBeliefs.size);
          var parent = 0;
          while(parent < parentBeliefs.size) {
            if(parentBeliefs(parent) >= maxParent + beamThreshold || parent == child) {
              // \mu_{parent->child)(w) = \sum_{wcon,winn} \mu(wcon,winn) (p(w_c|wcon)  etc
              scores(parent) = parentBeliefs(parent) + likelihoodTerm(child,parent);
            }
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
        }
        val result = child.foldLeft(new DualState(newChild))( (a,b) => (a * b).asInstanceOf[DualState]);
        result;
      }
    }

     case class DoubleDoubleEdge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistics],
                                parent: Option[DualState] = None, child: Option[DualState]=None) extends Edge with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = (parent,child) match {
        case (NullState,NullState) => copy(parent=None,child=None)
        case (NullState,x:DualState) => copy(parent=None,child=Some(x))
        case (y:DualState,x:DualState) => copy(parent=Some(y),child=Some(x))
        case (y:DualState,NullState) => copy(parent=Some(y),child=None)
        case _ => error("bad edge types!" + this + " " + parent + " " + child);
      }

      // not right. whatever.
      def score(a: Word, b: Word) = {
        edgeParams.summed(wordIndex(a),wordIndex(b));
      }

      lazy val partition = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val scores = negativeInfinityArray(parent.size * child.size);
        var p = 0;
        var i = 0;
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + likelihoodTerm(c,p)
                scores(i) = score;
              }
              i += 1;
              c += 1;
            }

          p += 1
        }
        Numerics.logSum(scores, i);
      }

      def posteriorInnovationProb = sufficientStatistics.probInnovation;
      def posteriorFromInnovativeParent = sufficientStatistics.fromInnovativeParent;


      lazy val sufficientStatistics: SufficientStatistics = {
        val parent = this.parent.get.beliefs;
        val parentMax = this.parent.get.max;
        val child = this.child.get.beliefs;
        val childMax = this.child.get.max;
        var p = 0;

        var wordChangeCounts = baseFactory.emptySufficientStatistics;
        var innovCounts = innovationFactory.emptySufficientStatistics;
        var pInnov = 0.0;
        var pFromInnovativeParent = 0.0;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + likelihoodTerm(c,p) - partition);
                // log p(new|wc,wp) (numerator)
                val parentCon = p / wordIndex.size
                val parentInn = p % wordIndex.size
                val childCon = c / wordIndex.size
                val childInn = c % wordIndex.size
                val posteriorInnovation = edgeParams.withInnovation(parentCon, childCon) + edgeParams.logInnov;
                // log p(not new|wc,wp) (numerator)
                val fromConservativeParent = edgeParams.logNonInnov + edgeParams.withoutInnovation(parentCon,childCon) + edgeParams.logParentConservative;
                val fromInnovativeParent = edgeParams.logNonInnov + edgeParams.logParentInnov + edgeParams.withoutInnovation(parentInn, childCon)
                val posteriorNormalizer = Numerics.logSum(posteriorInnovation, fromConservativeParent, fromInnovativeParent)
                // p(new|wc,wp) (normalized)
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - posteriorNormalizer);
                // p(from con parent| wc, wp)
                val normFromConParent = math.exp(fromConservativeParent - posteriorNormalizer);
                val normFromInnParent = math.exp(fromInnovativeParent - posteriorNormalizer);
                if(normFromConParent * score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(parentCon,childCon).result * (normFromConParent * score));
                if(normFromInnParent * score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(parentInn,childCon).result * (normFromInnParent * score));
                if(normalizedPosteriorInnovation * score > 1E-8) // counts for p and c weighted by p(new,wc,wp)
                  innovCounts += (innovBaseCounts(parentCon,childCon).result * (normalizedPosteriorInnovation * score));
                if(score > 1E-8)
                  innovCounts += (innovBaseCounts(parentCon,childInn).result * score);

                pInnov += normalizedPosteriorInnovation * score;
                pFromInnovativeParent += (normFromInnParent/(normFromInnParent + normFromConParent)) * score;

              }

              c += 1;
            }

          p += 1
        }


        SufficientStatistics(wordChangeCounts,innovCounts, pFromInnovativeParent, pInnov,1,true);
      }

      def likelihoodTerm(child: Int, parent:Int): Double = {
        val parentCon = parent / wordIndex.size
        val parentInn = parent % wordIndex.size
        val childCon = child / wordIndex.size
        val childInn = child % wordIndex.size
        val conPart =  Numerics.logSum(edgeParams.withoutInnovation(parentCon, childCon) + edgeParams.logParentConservative + edgeParams.logNonInnov,
          edgeParams.withoutInnovation(parentInn, childCon) + edgeParams.logParentInnov + edgeParams.logNonInnov,
          edgeParams.withInnovation(parentCon, childCon) + edgeParams.logInnov
        )
        val innPart = edgeParams.withInnovation(parentCon, childInn);
        conPart + innPart
      }

      def parentProjection:Belief = {
        val newParent = new DenseVector(wordIndex.size * wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val maxChild = this.child.get.max;
        var parent = 0;
        while(parent < newParent.size) {

          val scores = negativeInfinityArray(childBeliefs.size)
          var child = 0;
          while(child < childBeliefs.size) {
            if(childBeliefs(child) >= maxChild + beamThreshold || parent == child)
              scores(child) = childBeliefs(child) + (
                likelihoodTerm(child, parent)
              )
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          parent += 1;
        }
        val result = this.parent.foldLeft(new DualState(newParent))( (a,b) => (a * b).asInstanceOf[DualState]);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size * wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val maxParent = this.parent.get.max;
        for( child <- 0 until newChild.size) {
          val scores = negativeInfinityArray(parentBeliefs.size);
          var parent = 0;
          while(parent < parentBeliefs.size) {
            if(parentBeliefs(parent) >= maxParent + beamThreshold || parent == child) {
              scores(parent) = parentBeliefs(parent) + likelihoodTerm(child,parent);
            }
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
        }
        val result = child.foldLeft(new DualState(newChild))( (a,b) => (a * b).asInstanceOf[DualState]);
        result;
      }
    }

    case class DoubleSingleEdge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistics],
                                parent: Option[DualState] = None, child: Option[SingleState]=None) extends Edge with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = (parent,child) match {
        case (NullState,NullState) => copy(parent=None,child=None)
        case (NullState,x:SingleState) => copy(parent=None,child=Some(x))
        case (y:DualState,x:SingleState) => copy(parent=Some(y),child=Some(x))
        case (y:DualState,NullState) => copy(parent=Some(y),child=None)
        case _ => error("bad edge types!" + this + " " + parent + " " + child);
      }

      // not right. whatever.
      def score(a: Word, b: Word) = {
        edgeParams.summed(wordIndex(a),wordIndex(b));
      }

      lazy val partition = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val scores = negativeInfinityArray(parent.size * child.size);
        var p = 0;
        var i = 0;
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + likelihoodTerm(c,p)
                scores(i) = score;
              }
              i += 1;
              c += 1;
            }

          p += 1
        }
        Numerics.logSum(scores, i);
      }

      def posteriorInnovationProb = sufficientStatistics.probInnovation;
      def posteriorFromInnovativeParent = sufficientStatistics.fromInnovativeParent;


      lazy val sufficientStatistics: SufficientStatistics = {
        val parent = this.parent.get.beliefs;
        val parentMax = this.parent.get.max;
        val child = this.child.get.beliefs;
        val childMax = this.child.get.max;
        var p = 0;

        var wordChangeCounts = baseFactory.emptySufficientStatistics;
        var innovCounts = innovationFactory.emptySufficientStatistics;
        var pInnov = 0.0;
        var pFromInnovativeParent = 0.0;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + likelihoodTerm(c,p) - partition);
                // log p(new|wc,wp) (numerator)
                val parentCon = p / wordIndex.size
                val parentInn = p % wordIndex.size
                val posteriorInnovation = edgeParams.withInnovation(parentCon, c) + edgeParams.logInnov;
                // log p(not new|wc,wp) (numerator)
                val fromConservativeParent = edgeParams.logParentConservative + edgeParams.withoutInnovation(parentCon,c) + edgeParams.logNonInnov;
                val fromInnovativeParent = edgeParams.logParentInnov + edgeParams.logNonInnov + edgeParams.withoutInnovation(parentInn, c)
                val posteriorNormalizer = Numerics.logSum(posteriorInnovation, fromConservativeParent, fromInnovativeParent)
                // p(new|wc,wp) (normalized)
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - posteriorNormalizer);
                // p(from con parent| wc, wp)
                val normFromConParent = math.exp(fromConservativeParent - posteriorNormalizer);
                val normFromInnParent = math.exp(fromInnovativeParent - posteriorNormalizer);
                if(normFromConParent * score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(parentCon,c).result * (normFromConParent * score));
                if(normFromInnParent * score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(parentInn,c).result * (normFromInnParent * score));
                if(normalizedPosteriorInnovation * score > 1E-8) // counts for p and c weighted by p(new,wc,wp)
                  innovCounts += (innovBaseCounts(parentCon,c).result * (normalizedPosteriorInnovation * score));


                pInnov += normalizedPosteriorInnovation * score;
                pFromInnovativeParent += normFromInnParent * score;

              }

              c += 1;
            }

          p += 1
        }


        SufficientStatistics(wordChangeCounts,innovCounts, pFromInnovativeParent, pInnov,1, false);
      }

      def likelihoodTerm(child: Int, parent:Int): Double = {
        val parentCon = parent / wordIndex.size
        val parentInn = parent % wordIndex.size
        Numerics.logSum(edgeParams.withoutInnovation(parentCon, child) + edgeParams.logParentConservative + edgeParams.logNonInnov,
          edgeParams.withoutInnovation(parentInn, child) + edgeParams.logParentInnov + edgeParams.logNonInnov,
          edgeParams.withInnovation(parentCon, child) + edgeParams.logInnov
        )
      }

      def parentProjection:Belief = {
        val newParent = new DenseVector(wordIndex.size * wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val maxChild = this.child.get.max;
        var parent = 0;
        while(parent < newParent.size) {

          val scores = negativeInfinityArray(wordIndex.size);
          var child = 0;
          while(child <  childBeliefs.size) {
            if(childBeliefs(child) >= maxChild + beamThreshold || parent == child)
              // \mu_{child->parent}(wcon,winn)
               //   = \sum_{w_c} \mu_{wc} (p(w_c|wcon) * p(nonInnov) + p(w_c|winn) * p(innov)) * p(nonInnov) + p(w_c|lm) * p(innov)
              scores(child) = childBeliefs(child) + (
                likelihoodTerm(child, parent)
              )
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          parent += 1;
        }
        val result = this.parent.foldLeft(new DualState(newParent))( (a,b) => (a * b).asInstanceOf[DualState]);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val maxParent = this.parent.get.max;
        for( child <- 0 until newChild.size) {
          val scores = negativeInfinityArray(parentBeliefs.size);
          var parent = 0;
          while(parent < parentBeliefs.size) {
            if(parentBeliefs(parent) >= maxParent + beamThreshold || parent == child) {
              // \mu_{parent->child)(w) = \sum_{wcon,winn} \mu(wcon,winn) (p(w_c|wcon)  etc
              scores(parent) = parentBeliefs(parent) + likelihoodTerm(child,parent);
            }
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
        }
        val result = child.foldLeft(new SingleState(newChild))( (a,b) => (a * b).asInstanceOf[SingleState]);
        result;
      }
    }


    case class SingleSingleEdge(edgeParams: EdgeParams,
                                baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
                                innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistics],
                                parent: Option[SingleState] = None,
                                child: Option[SingleState] = None) extends Edge with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = (parent,child) match {
        case (NullState,NullState) => copy(parent=None,child=None)
        case (NullState,x:SingleState) => copy(parent=None,child=Some(x))
        case (y:SingleState,x:SingleState) => copy(parent=Some(y),child=Some(x))
        case (y:SingleState,NullState) => copy(parent=Some(y),child=None)
        case _ => error("bad edge types!" + this + " " + parent + " " + child);
      }

      def score(a: Word, b: Word) = {
        edgeParams.summed(wordIndex(a),wordIndex(b));
      }

      def sufficientStatistics: SufficientStatistics = {
        val parent = this.parent.get.beliefs;
        val parentMax = this.parent.get.max;
        val child = this.child.get.beliefs;
        val childMax = this.child.get.max;
        var p = 0;

        var wordChangeCounts = baseFactory.emptySufficientStatistics;
        var innovCounts = innovationFactory.emptySufficientStatistics;
        var pInnov = 0.0;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + edgeParams.summed(p,c) - partition);
                // log p(new|wc,wp) (numerator)
                val posteriorInnovation = edgeParams.withInnovation(p, c) + edgeParams.logInnov;
                // log p(not new|wc,wp) (numerator)
                val nonInnovation = edgeParams.withoutInnovation(p,c) + edgeParams.logNonInnov;
                // p(new|wc,wp) (normalized)
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation));
                val normNonInnov = 1-normalizedPosteriorInnovation;
                if(normNonInnov * score > 1E-8) // counts for p and c weighted by p(not new,wc,wp)
                  wordChangeCounts += (baseCounts(p,c).result * (normNonInnov * score));
                if(normalizedPosteriorInnovation * score > 1E-8) // counts for p and c weighted by p(new,wc,wp)
                  innovCounts += (innovBaseCounts(p,c).result * (normalizedPosteriorInnovation * score));


                pInnov += normalizedPosteriorInnovation * score;
              }

              c += 1;
            }

          p += 1
        }

        SufficientStatistics(wordChangeCounts,innovCounts, 0.0, pInnov,1, false);
      }


      def posteriorFromInnovativeParent = 0.0

      lazy val posteriorInnovationProb = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        var p = 0;

        var pInnov = 0.0;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                // p(wc,wp)
                val score = math.exp(parent(p) + child(c) + edgeParams.summed(p,c) - partition);
                // log p(new|wc,wp) (numerator)
                val posteriorInnovation = edgeParams.withInnovation(p, c) + edgeParams.logInnov;
                // log p(not new|wc,wp) (numerator)
                val nonInnovation = edgeParams.withoutInnovation(p,c) + edgeParams.logNonInnov;
                // p(new|wc,wp) (normalized)
                val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation));

                pInnov += normalizedPosteriorInnovation * score;
              }

              c += 1;
            }

          p += 1
        }

        pInnov;
      }

      lazy val partition = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val scores = negativeInfinityArray(parent.size * child.size);
        var p = 0;
        var i = 0;
        val parentMax = this.parent.get.max;
        val childMax = this.child.get.max;
        while(p < parent.size) {
          var c = 0;
          if(parent(p) >= parentMax + beamThreshold)
            while(c < child.size) {
              if(child(c) >= childMax + beamThreshold) {
                val score = parent(p) + child(c) + edgeParams.summed(p,c)
                scores(i) = score;
              }
              i += 1;
              c += 1;
            }

          p += 1
        }
        Numerics.logSum(scores, i);
      }



      def parentProjection:Belief = { // just a matrix multiply in log space.
        val newParent = new DenseVector(wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val maxChild = this.child.get.max;
        var parent = 0;
        while(parent < newParent.size) {
          val scores = negativeInfinityArray(wordIndex.size);
          var child = 0;
          while(child < childBeliefs.size) {
            if(childBeliefs(child) >= maxChild + beamThreshold || parent == child)
              scores(child) = childBeliefs(child) + edgeParams.summed(parent,child);
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          parent += 1;
        }
        val result = this.parent.foldLeft(new SingleState(newParent))( (a,b) => (a * b).asInstanceOf[SingleState]);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val maxParent = this.parent.get.max;
        for( child <- 0 until newChild.size) {
          val scores = negativeInfinityArray(wordIndex.size);
          var parent = 0;
          while(parent < parentBeliefs.size) {
            if(parentBeliefs(parent) >= maxParent + beamThreshold || parent == child)
              scores(parent) = parentBeliefs(parent) + edgeParams.summed(parent,child);
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
        }
        val result = child.foldLeft(new SingleState(newChild))( (a,b) => (a * b).asInstanceOf[SingleState]);
        result;
      }
    }
    case class EdgeParams(summed: ArrayCache,
                          withoutInnovation: ArrayCache,
                          withInnovation: ArrayCache,
                          logInnov: Double, logNonInnov: Double,
                          logParentInnov: Double, logParentConservative: Double);

    import collection.{mutable=>m}
    private val precomputedCosts = new m.HashMap[(Language,Language),EdgeParams] with m.SynchronizedMap[(Language,Language),EdgeParams];
    private val precomputedECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[baseFactory.SufficientStatistics]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[baseFactory.SufficientStatistics]];
    private val precomputedIECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[innovationFactory.SufficientStatistics]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[innovationFactory.SufficientStatistics]];
    private def computeCosts(edge: baseFactors.Edge, innovEdge: innovationFactors.Edge, innovationProb: Double, parentProb: Double) = {
      val withoutInnovation = new ArrayCache(wordIndex.size,wordIndex.size)( {(p,c) =>
        edge.score(wordIndex.get(p),wordIndex.get(c));
      })
      val logProbInnovation = math.log(innovationProb);
      val nonInnovation = math.log(1-innovationProb);
      val logProbParentInnovation = math.log(parentProb)
      val logProbParentConservative = math.log(1-parentProb)

      val withInnovation = new ArrayCache(wordIndex.size,wordIndex.size)( {(p,c) =>
        innovEdge.score(wordIndex.get(p),wordIndex.get(c));
      });

      val summed = new ArrayCache(wordIndex.size,wordIndex.size) ({ (p,c) =>
        if(viterbi) Numerics.logSum(withoutInnovation(p,c) + nonInnovation,withInnovation(p, c) + logProbInnovation);
        else math.max(withoutInnovation(p,c) + nonInnovation,withInnovation(p, c) + logProbInnovation)
      })

      EdgeParams(summed,withoutInnovation,withInnovation, logProbInnovation, nonInnovation, logProbParentInnovation, logProbParentConservative);
    }

    private def computeECounts(edge: baseFactors.Edge): ((Int, Int) => Lazy[baseFactory.SufficientStatistics]) = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        lazy val marg = edge.edgeMarginal(baseFactors.beliefForWord(wordIndex.get(p)),baseFactors.beliefForWord(wordIndex.get(c)));
        Lazy.delay(marg.sufficientStatistics);
      };
      {(a:Int,b:Int) => expectedCountsForWords(a)(b)}
    }

    private def computeIECounts(edge: innovationFactors.Edge): ((Int, Int) => Lazy[innovationFactory.SufficientStatistics]) = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        lazy val marg = edge.edgeMarginal(innovationFactors.beliefForWord(wordIndex.get(p)),innovationFactors.beliefForWord(wordIndex.get(c)));
        Lazy.delay(marg.sufficientStatistics);
      };
      {(a:Int,b:Int) => expectedCountsForWords(a)(b)}
    }
  }

  def negativeInfinityArray(size: Int): Array[Double] = {
    val r = new Array[Double](size);
    Arrays.fill(r,Double.NegativeInfinity);
    r;
  }

}