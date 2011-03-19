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

class ParsimonyFactorsFactory(val factory:AutomatonFactory[Char]) {
  import factory.{EditDistance => _, _};
  import EditDistance._;

  case class EdgeExpectedCounts(alignmentCounts: Vector, probInnovation: Double, n: Int) {
    def +(that: EdgeExpectedCounts) = {
      EdgeExpectedCounts(alignmentCounts + that.alignmentCounts, probInnovation + that.probInnovation, n + that.n);
    }
  }
  /**
   *
   * @author dlwh
   */
  class WordFactors(legalWords: Set[Word],
                    costMatrix: (Language,Language)=>((Char,Char)=>Double),
                    innovationProbability: (Language,Language)=>Double,
                    viterbi: Boolean = false)extends Factors {
    def this(legalWords: Set[Word], subCost: Double,
             delCost: Double,
             innovationProbability: Double)= this(legalWords,((_,_)=>simpleCostMatrix(index.size-1,subCost, delCost)),(_,_)=>innovationProbability);

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
    private val precomputedECounts = new m.HashMap[(Language,Language),(Int,Int)=>Lazy[Vector]] with m.SynchronizedMap[(Language,Language),(Int,Int)=>Lazy[Vector]];

    def edgeFor(parent: Language, child: Language) = {
      val matrix = precomputedCosts.getOrElseUpdate(parent->child,computeCosts(costMatrix(parent,child),innovationProbability(parent,child)));
      def expCosts(wordA: Int, wordB: Int)  = precomputedECounts.getOrElseUpdate(parent->child,computeECounts(costMatrix(parent,child)))(wordA,wordB);
      new Edge(matrix,expCosts _, None,None);
    }

    case class EdgeParams(summed: DenseMatrix, withoutInnovation: DenseMatrix, withInnovation: DenseVector, logInnov: Double, logNonInnov: Double);

    private def computeCosts(matrix: (Char,Char)=>Double, innovationProb: Double) = {
      val summed = new DenseMatrix(wordIndex.size, wordIndex.size);
      val withoutInnovation: DenseMatrix = new DenseMatrix(wordIndex.size, wordIndex.size);
      val withInnovation = new DenseVector(wordIndex.size);
      for(i <- 0 until legalWords.size)
        withInnovation(i) =  wordIndex.get(i).length * -math.log(index.size)
      val logProbInnovation = math.log(innovationProb);
      val nonInnovation = math.log(1-innovationProb);

      val dv = new DenseVector(wordIndex.size);
      for( i <- 0 until legalWords.size) {
        for(j <- 0 until legalWords.size) {
          dv(j) = editDistance(wordIndex.get(i),wordIndex.get(j),matrix);
        }
        //logNormalizeInPlace(dv);
        for(j <- 0 until legalWords.size) {
          summed(i,j) = Numerics.logSum(dv(j) + nonInnovation,withInnovation(j) + logProbInnovation);
          withoutInnovation(i,j) = dv(j);
        }
      }
      EdgeParams(summed,withoutInnovation,withInnovation, logProbInnovation, nonInnovation);
    }

    private def computeECounts(matrix: (Char,Char)=>Double): (Int, Int) => Lazy[Vector] = {
      val expectedCountsForWords = Array.tabulate(wordIndex.size,wordIndex.size) { (p,c) =>
        Lazy.delay(EditDistance.expectedCounts(index,wordIndex.get(p),wordIndex.get(c), matrix) : Vector)
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



    class Edge(edgeParams: EdgeParams, baseCounts: (Int,Int)=>Lazy[Vector],
               val parent: Option[Belief]=None,
               val child: Option[Belief]=None) extends EdgeBase {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(edgeParams,baseCounts,Some(parent),Some(child));
      }

      def expectedCounts = {
        val parent = this.parent.get.beliefs;
        val child = this.child.get.beliefs;
        var p = 0;

        val wordChangeCounts = new SparseVector(index.size * index.size);
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

        EdgeExpectedCounts(wordChangeCounts,pInnov,1);
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