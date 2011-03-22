package dlwh.newcognates

import scalanlp.fst.fast.AutomatonFactory
import scalala.Scalala.{iFigure => _, _scalala_figure => _, _}
import scalanlp.math.Numerics
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.tensor.Vector;
import scalala.tensor.sparse.SparseVector
import scalanlp.util.{Lazy, Encoder, Index}

/**
 * 
 * @author dlwh
 */

class ParsimonyFactorsFactory[Factory<:SuffStatsFactorsFactory](val baseFactory: Factory, initialInnovationProb: Double, baseInnovationProb: Double) extends SuffStatsFactorsFactory {



  case class EdgeParameters(params: baseFactory.EdgeParameters, innovationProb: Double);

  case class SufficientStatistics(alignmentCounts: baseFactory.SufficientStatistics,
                                  probInnovation: Double,
                                  n: Double) extends BaseSufficientStatistics {
    def +(that: SufficientStatistics) = {
      SufficientStatistics(alignmentCounts + that.alignmentCounts, probInnovation + that.probInnovation, n + that.n);
    }
    def *(weight: Double) = {
      SufficientStatistics(alignmentCounts * weight, probInnovation * weight, n * weight);
    }
  }


  def initialParameters = new EdgeParameters(baseFactory.initialParameters,initialInnovationProb);
  def emptySufficientStatistics = SufficientStatistics(baseFactory.emptySufficientStatistics,0,0);

  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters) =  {
    val factors = baseFactory.mkFactors(legalWords, edgeParameters andThen (_.params))
    new Factors(legalWords,factors,edgeParameters andThen (_.innovationProb));
  }

  def optimize(stats: Map[Language,SufficientStatistics]) = {
    val newInnerParams = baseFactory.optimize(stats.mapValues(_.alignmentCounts));
    newInnerParams.mapValues(inner => EdgeParameters(inner,baseInnovationProb));
  }

  /**
   *
   * @author dlwh
   */
  class Factors(legalWords: Set[Word], baseFactors: baseFactory.Factors,
                innovationProb: (Language)=>Double,
                viterbi: Boolean = false) extends FactorsWithSuffStats {
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
    private val precomputedCosts = new m.HashMap[(Language,Language),EdgeParams] with m.SynchronizedMap[(Language,Language),EdgeParams];
    private val precomputedECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[baseFactory.SufficientStatistics]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[baseFactory.SufficientStatistics]];

    def edgeFor(parent: Language, child: Language) = {
      val edge = baseFactors.edgeFor(parent, child)
      val matrix = precomputedCosts.getOrElseUpdate(parent->child,computeCosts(edge,innovationProb(child)));
      def expCounts(wordA: Int, wordB: Int)  = {
        precomputedECounts.getOrElseUpdate(parent -> child, computeECounts(edge))(wordA, wordB)
      };
      new Edge(matrix,expCounts _, None,None);
    }

    case class EdgeParams(summed: DenseMatrix, withoutInnovation: DenseMatrix, withInnovation: DenseVector, logInnov: Double, logNonInnov: Double);

    private def computeCosts(edge: baseFactors.Edge, innovationProb: Double) = {
      val summed = new DenseMatrix(wordIndex.size, wordIndex.size);
      val withoutInnovation: DenseMatrix = new DenseMatrix(wordIndex.size, wordIndex.size);
      val withInnovation = new DenseVector(wordIndex.size);
      for(i <- 0 until legalWords.size)
        withInnovation(i) =  -7.3 //wordIndex.get(i).length * -math.log(index.size)
      val logProbInnovation = math.log(innovationProb);
      val nonInnovation = math.log(1-innovationProb);

      val dv = new DenseVector(wordIndex.size);
      for( i <- 0 until legalWords.size) {
        for(j <- 0 until legalWords.size) {
          dv(j) = edge.score(wordIndex.get(i),wordIndex.get(j));
        }
        //logNormalizeInPlace(dv);
        for(j <- 0 until legalWords.size) {
          summed(i,j) = Numerics.logSum(dv(j) + nonInnovation,withInnovation(j) + logProbInnovation);
          withoutInnovation(i,j) = dv(j);
        }
      }
      EdgeParams(summed,withoutInnovation,withInnovation, logProbInnovation, nonInnovation);
    }

    private def computeECounts(edge: baseFactors.Edge): ((Int, Int) => Lazy[baseFactory.SufficientStatistics]) = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        lazy val marg = edge.edgeMarginal(baseFactors.beliefForWord(wordIndex.get(p)),baseFactors.beliefForWord(wordIndex.get(c)));
        Lazy.delay(marg.sufficientStatistics);
      };
      {(a:Int,b:Int) => expectedCountsForWords(a)(b)}
    }

    case class Belief(beliefs: DenseVector) extends BeliefBase {
      lazy val partition = Numerics.logSum(beliefs);
      def apply(word: String)= beliefs(wordIndex(word));

      def /(b: Belief):Belief = {
        val diff = beliefs -b.beliefs value;
        new Belief(diff);
      }

      def *(b: Belief):Belief = {
        val r = new Belief(beliefs + b.beliefs value);
        r
      }

      override def toString() = {
        val ctr = (Encoder.fromIndex(wordIndex).decode(beliefs));
        "Belief: " + ctr.maxk(2).map(k => k -> ctr(k));
      };

      def normalized = scaleBy(-partition)

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score value);
        new Belief(scaled);
      }

    }



    class Edge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[baseFactory.SufficientStatistics],
               val parent: Option[Belief]=None,
               val child: Option[Belief]=None) extends EdgeBase with HasSufficientStatistics {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(edgeParams,baseCounts,Some(parent),Some(child));
      }

      def score(a: Word, b: Word) = {
        edgeParams.summed(wordIndex(a),wordIndex(b));
      }

      def sufficientStatistics = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        var p = 0;

        var wordChangeCounts = baseFactory.emptySufficientStatistics;
        var pInnov = 0.0;
        while(p < parent.size) {
          var c = 0;
          while(c < child.size) {
            // p(wc,wp)
            val score = math.exp(parent(p) + child(c) + edgeParams.summed(p,c) - partition);
            // log p(new|wc,wp) (numerator)
            val posteriorInnovation = edgeParams.withInnovation(c) + edgeParams.logInnov;
            // log p(not new|wc,wp) (numerator)
            val nonInnovation = edgeParams.withoutInnovation(p,c) + edgeParams.logNonInnov;
            // p(new|wc,wp) (normalized)
            val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation));
            val normNonInnov = 1-normalizedPosteriorInnovation;
            if(normNonInnov * score > 1E-5) // counts for p and c weighted by p(not new,wc,wp)
              wordChangeCounts += (baseCounts(p,c).result * (normNonInnov * score));

            pInnov += normalizedPosteriorInnovation * score;

            c += 1;
          }

          p += 1
        }

        SufficientStatistics(wordChangeCounts,pInnov,1);
      }

      def posteriorInnovationProb = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        var p = 0;

        var pInnov = 0.0;
        while(p < parent.size) {
          var c = 0;
          while(c < child.size) {
            // p(wc,wp)
            val score = math.exp(parent(p) + child(c) + edgeParams.summed(p,c) - partition);
            // log p(new|wc,wp) (numerator)
            val posteriorInnovation = edgeParams.withInnovation(c) + edgeParams.logInnov;
            // log p(not new|wc,wp) (numerator)
            val nonInnovation = edgeParams.withoutInnovation(p,c) + edgeParams.logNonInnov;
            // p(new|wc,wp) (normalized)
            val normalizedPosteriorInnovation = math.exp(posteriorInnovation - Numerics.logSum(posteriorInnovation,nonInnovation));
            pInnov += normalizedPosteriorInnovation * score;

            c += 1;
          }

          p += 1
        }

        pInnov;
      }

      lazy val partition = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        val scores = Array.fill(parent.size * child.size) { Double.NegativeInfinity};
        var p = 0;
        var i = 0;
        while(p < parent.size) {
          var c = 0;
          while(c < child.size) {
            val score = parent(p) + child(c) + edgeParams.summed(p,c)
            scores(i) = score;
            i += 1;
            c += 1;
          }

          p += 1
        }
        Numerics.logSum(scores);
      }



      def parentProjection:Belief = { // just a matrix multiply in log space.
        val newParent = new DenseVector(wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val scores = Array.fill(childBeliefs.size)(Double.NegativeInfinity);
        for( parent <- 0 until wordIndex.size) {
          var child = 0;
          while(child < wordIndex.size) {
            scores(child) = childBeliefs(child) + edgeParams.summed(parent,child);
            child += 1;
          }
          newParent(parent) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
        }
        val result = parent.foldLeft(new Belief(newParent))(_ * _);
        result;
      }

      def childProjection:Belief = {
        val newChild = new DenseVector(wordIndex.size);
        val parentBeliefs = this.parent.get.beliefs;
        val scores = Array.fill(parentBeliefs.size)(Double.NegativeInfinity);
        for( child <- 0 until wordIndex.size) {
          var parent = 0;
          while(parent < wordIndex.size) {
            scores(parent) = parentBeliefs(parent) + edgeParams.summed(parent,child);
            parent += 1;
          }
          newChild(child) = if(viterbi) Numerics.max(scores) else Numerics.logSum(scores);
        }
        val result = child.foldLeft(new Belief(newChild))(_ * _);
        result;
      }
    }
  }
}