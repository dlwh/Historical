package dlwh.newcognates

import scalala.Scalala.{iFigure => _, _scalala_figure => _, _}
import scalanlp.math.Numerics
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalanlp.util.{Lazy, Encoder, Index}
import java.util.Arrays
import scalala.tensor.counters.Counters
import scalanlp.optimize._

/**
 * 
 * @author dlwh
 */

class ParsimonyFactorsFactory[Factory<:SuffStatsFactorsFactory,
                              IF<:SuffStatsFactorsFactory](val baseFactory: Factory,
                                                           val innovationFactory: IF,
                                                           initialInnovationProb: Double,
                                                           baseInnovationProb: Double,
                                                           beamThreshold:Double = -10,
                                                           viterbi: Boolean = false) extends SuffStatsFactorsFactory {



  case class EdgeParameters(params: baseFactory.EdgeParameters, innovParams: innovationFactory.EdgeParameters, innovationProb: Double);

  case class SufficientStatistics(alignmentCounts: baseFactory.SufficientStatistics,
                                  innovationCounts: innovationFactory.SufficientStatistics,
                                  probInnovation: Double,
                                  n: Double) extends BaseSufficientStatistics {
    def +(that: SufficientStatistics) = {
      SufficientStatistics(alignmentCounts + that.alignmentCounts,
        innovationCounts + that.innovationCounts,
        probInnovation + that.probInnovation,
        n + that.n);
    }
    def *(weight: Double) = {
      SufficientStatistics(alignmentCounts * weight, innovationCounts * weight, probInnovation * weight, n * weight);
    }
  }


  def initialParameters = new EdgeParameters(baseFactory.initialParameters,
    innovationFactory.initialParameters,
    initialInnovationProb);
  def emptySufficientStatistics = SufficientStatistics(baseFactory.emptySufficientStatistics,innovationFactory.emptySufficientStatistics,0,0);

  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters) =  {
    val factors = baseFactory.mkFactors(legalWords, edgeParameters andThen (_.params))
    val innovFactors = innovationFactory.mkFactors(legalWords, edgeParameters andThen (_.innovParams));
    new Factors(legalWords,factors, innovFactors ,edgeParameters andThen (_.innovationProb));
  }

  def optimize(stats: Map[Language,SufficientStatistics]) = {
    val newInnerParams = baseFactory.optimize(stats.mapValues(_.alignmentCounts));
    val newInnovParams = innovationFactory.optimize(stats.mapValues(_.innovationCounts));
    val innoProbs = optimizeInnovation(stats.mapValues(s => (s.probInnovation,s.n)))
    println(innoProbs);
    newInnerParams.map{case (k,inner) => k -> EdgeParameters(inner,newInnovParams(k), innoProbs(k) )};
  }

  def optimizeInnovation(stats: Map[Language,(Double,Double)]) = {
    val obj = new InnovationObjective(stats);
    val opt = FirstOrderMinimizer.OptParams(regularization = 1E-4, useStochastic = false, maxIterations = 50);
    val result = opt.minimizer(obj).minimize(obj, obj.enc.mkDenseVector());
    obj.extractProbs(result);
  }

  /**
   *
   * @author dlwh
   */
  class Factors(legalWords: Set[Word], baseFactors: baseFactory.Factors,
                innovationFactors: innovationFactory.Factors,
                innovationProb: (Language)=>Double) extends FactorsWithSuffStats {
    val wordIndex = Index(legalWords);
    def initialBelief(lang: Language) = new Belief(allOnes,0.0);

    def initialMessage(from: Language, to: Language) = new Belief(allOnes,0.0);

    def beliefForWord(w: Word) = new Belief({
      val r = Encoder.fromIndex(wordIndex).mkDenseVector(Double.NegativeInfinity);
      r(wordIndex(w)) = 0.0;
      r
    },0.0)
    val allOnes = new DenseVector(wordIndex.size);

    val rootMessage = {
      val allOnes =logNormalizeInPlace(new DenseVector(wordIndex.size));
      new Belief(allOnes,allOnes(0));
    }

    private def logNormalizeInPlace(v: DenseVector) = {
      v -= Numerics.logSum(v);
      v
    }

    import collection.{mutable=>m}
    private val precomputedCosts = new m.HashMap[(Language,Language),EdgeParams] with m.SynchronizedMap[(Language,Language),EdgeParams];
    private val precomputedECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[baseFactory.SufficientStatistics]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[baseFactory.SufficientStatistics]];
    private val precomputedIECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[innovationFactory.SufficientStatistics]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[innovationFactory.SufficientStatistics]];

    def edgeFor(parent: Language, child: Language) = {
      val edge = baseFactors.edgeFor(parent, child)
      val innovEdge = innovationFactors.edgeFor(parent,child);
      val matrix = precomputedCosts.getOrElseUpdate(parent->child,computeCosts(edge, innovEdge, innovationProb(child)));
      def expCounts(wordA: Int, wordB: Int)  = {
        precomputedECounts.getOrElseUpdate(parent -> child, computeECounts(edge))(wordA, wordB)
      };
      def innovExpCounts(wordA: Int, wordB: Int)  = {
        precomputedIECounts.getOrElseUpdate(parent -> child, computeIECounts(innovEdge))(wordA, wordB)
      };
      new Edge(matrix,expCounts _, innovExpCounts _, None,None);
    }

    case class EdgeParams(summed: ArrayCache, withoutInnovation: ArrayCache, withInnovation: ArrayCache, logInnov: Double, logNonInnov: Double);

    private def computeCosts(edge: baseFactors.Edge, innovEdge: innovationFactors.Edge, innovationProb: Double) = {
      val withoutInnovation = new ArrayCache(wordIndex.size,wordIndex.size)( {(p,c) =>
        edge.score(wordIndex.get(p),wordIndex.get(c));
      })
      val logProbInnovation = math.log(innovationProb);
      val nonInnovation = math.log(1-innovationProb);

      val withInnovation = new ArrayCache(wordIndex.size,wordIndex.size)( {(p,c) =>
        innovEdge.score(wordIndex.get(p),wordIndex.get(c));
      });

      val summed = new ArrayCache(wordIndex.size,wordIndex.size) ({ (p,c) =>
        if(viterbi) Numerics.logSum(withoutInnovation(p,c) + nonInnovation,withInnovation(p, c) + logProbInnovation);
        else math.max(withoutInnovation(p,c) + nonInnovation,withInnovation(p, c) + logProbInnovation)
      })

      EdgeParams(summed,withoutInnovation,withInnovation, logProbInnovation, nonInnovation);
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

    case class Belief(beliefs: DenseVector, max: Double) extends BeliefBase {
      lazy val partition = { val r = Numerics.logSum(beliefs); assert(!r.isNaN && !r.isInfinite); r}
      def apply(word: String)= beliefs(wordIndex(word));

      def /(b: Belief):Belief = {
        val newBeliefs = beliefs - b.beliefs value
        val newMax = Numerics.max(newBeliefs.data);
        new Belief(newBeliefs, newMax);
      }

      def *(b: Belief):Belief = {
        val newBeliefs = beliefs + b.beliefs value
        val newMax = Numerics.max(newBeliefs.data);
        val r = new Belief(newBeliefs, newMax);
        r
      }

      override def toString() = {
        val ctr = (Encoder.fromIndex(wordIndex).decode(beliefs));
        "Belief: " + ctr.maxk(2).map(k => k -> ctr(k));
      };

      def normalized = scaleBy(-partition)

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score value);
        new Belief(scaled, max + score);
      }

    }



    class Edge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
               innovBaseCounts: (Int,Int)=>Lazy[innovationFactory.SufficientStatistics],
               val parent: Option[Belief]=None,
               val child: Option[Belief]=None) extends EdgeBase with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(edgeParams,baseCounts, innovBaseCounts, Some(parent),Some(child));
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
        if(!(1/(1-pInnov)).isInfinite)
          wordChangeCounts *= 1/(1-pInnov);
        if(!(1/(pInnov)).isInfinite)
          innovCounts *=  1 /(pInnov);


        SufficientStatistics(wordChangeCounts,innovCounts, pInnov,1);
      }

      def posteriorInnovationProb = {
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
        var newMax = Double.NegativeInfinity
        var parent = 0;
        while(parent < newParent.size) {
          val scores = negativeInfinityArray(wordIndex.size);
          var child = 0;
          while(child < wordIndex.size) {
            if(childBeliefs(child) >= maxChild + beamThreshold || parent == child)
              scores(child) = childBeliefs(child) + edgeParams.summed(parent,child);
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          newMax = newMax max newParent(parent);
          parent += 1;
        }
        val result = this.parent.foldLeft(new Belief(newParent, newMax))(_ * _);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val maxParent = this.parent.get.max;
        var newMax = Double.NegativeInfinity
        for( child <- 0 until wordIndex.size) {
          val scores = negativeInfinityArray(wordIndex.size);
          var parent = 0;
          while(parent < wordIndex.size) {
            if(parentBeliefs(parent) >= maxParent + beamThreshold || parent == child)
              scores(parent) = parentBeliefs(parent) + edgeParams.summed(parent,child);
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
          newMax = newMax max newChild(child);
        }
        val result = child.foldLeft(new Belief(newChild, newMax))(_ * _);
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

class InnovationObjective(innovationCounts: Map[Language,(Double,Double)]) extends DiffFunction[Int,DenseVector] {
  val features = Index(innovationCounts.keysIterator ++ Iterator.single("GLOBAL"));
  val enc = Encoder.fromIndex(features);
  val globalFeature = features("GLOBAL");
  assert(globalFeature != -1);
  val encodedYes = enc.encodeDense(Counters.aggregate(innovationCounts.mapValues(_._1)));
  val encodedTotal = enc.encodeDense(Counters.aggregate(innovationCounts.mapValues(_._2)));
  def calculate(x: DenseVector) = {
    var ll = 0.0;
    val grad = x.like;

    var lang = 0;
    while(lang < encodedYes.size) {
      assert(encodedYes(lang) <= encodedTotal(lang));
      if(lang != globalFeature) {
        val pYes = Numerics.sigmoid(x(lang) + x(globalFeature));
        ll -= (math.log(pYes) * encodedYes(lang) + math.log(1-pYes) * (encodedTotal(lang) - encodedYes(lang)));
        val yesGradPart = encodedYes(lang) * (1 - pYes);
        val noGradPart = -(encodedTotal(lang) - encodedYes(lang)) * pYes;
        val gradContribution = yesGradPart + noGradPart;
        grad(lang) -= gradContribution;
        grad(globalFeature) -= gradContribution;
      }
      lang += 1;
    }

    (ll,grad);
  }

  def extractProbs(x: DenseVector): Map[Language, Double] = {
    val rawArray =Array.tabulate(innovationCounts.size) { lang =>  Numerics.sigmoid(x(lang) + x(globalFeature))};
    enc.decode(rawArray);
  }

}

