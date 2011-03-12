package dlwh.newcognates

import scalanlp.fst.fast.AutomatonFactory
import scalala.Scalala.{iFigure => _, _scalala_figure => _, _}
import scalala.tensor.counters.LogCounters.{LogDoubleCounter,LogPairedDoubleCounter}
import scalala.tensor.counters.LogCounters.{aggregate,logNormalize,logNormalizeRows}
import scalanlp.math.Numerics
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalanlp.util.{Encoder, Index}

/**
 * 
 * @author dlwh
 */

class WordFactorsFactory(val factory:AutomatonFactory[Char]) {
  import factory.{EditDistance => _, _};
  import EditDistance._;


  /**
   *
   * @author dlwh
   */
  class WordFactors(legalWords: Set[Word],
                    costMatrix: (Language,Language)=>((Char,Char)=>Double))extends Factors {
    def this(legalWords: Set[Word], subCost: Double, delCost: Double)= this(legalWords,((_,_)=>simpleCostMatrix(index.size-1,subCost, delCost)))

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
      new Belief(logNormalizeInPlace(allOnes));
    }

    def logNormalizeInPlace(v: DenseVector) = {
      v -= Numerics.logSum(v);
      v
    }

    import collection.{mutable=>m}
    private val precomputedCosts = new m.HashMap[(Language,Language),DenseMatrix] with m.SynchronizedMap[(Language,Language),DenseMatrix];

    def edgeFor(parent: Language, child: Language) = {
      val matrix = precomputedCosts.getOrElseUpdate(parent->child,computeCosts(costMatrix(parent,child)));
      new Edge(matrix,None,None);
    }

    private def computeCosts(matrix: (Char,Char)=>Double) = {
      val result = new DenseMatrix(wordIndex.size, wordIndex.size);
      val dv = new DenseVector(wordIndex.size);
      for( i <- 0 until legalWords.size) {
        for(j <- 0 until legalWords.size) {
          dv(j) = editDistance(wordIndex.get(i),wordIndex.get(j),matrix);
        }
        logNormalizeInPlace(dv);
        for(j <- 0 until legalWords.size) {
          result(i,j) = dv(j);
        }
      }
      result;
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

      override def toString() = ("Belief: " + Encoder.fromIndex(wordIndex).decode(beliefs));

      def normalized = scaleBy(-partition)

      def scaleBy(score: Double) = {
        val scaled = (beliefs + score value);
        new Belief(scaled);
      }

    }

    class Edge(costs: DenseMatrix, val parent: Option[Belief]=None, val child: Option[Belief]=None) extends EdgeBase {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(costs,Some(parent),Some(child));
      }

      def parentProjection:Belief = { // just a matrix multiply in log space.
        val newParent = new DenseVector(wordIndex.size);
        val childBeliefs = this.child.get.beliefs
        val scores = Array.fill(childBeliefs.size)(Double.NegativeInfinity);
        for( parent <- 0 until costs.rows) {
          var child = 0;
          while(child < wordIndex.size) {
            scores(child) = childBeliefs(child) + costs(parent,child);
            child += 1;
          }
          newParent(parent) = Numerics.logSum(scores);
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
            scores(parent) = parentBeliefs(parent) + costs(parent,child);
            parent += 1;
          }
          newChild(child) = Numerics.logSum(scores);
        }
        val result = child.foldLeft(new Belief(newChild))(_ * _);
        result;
      }
    }
  }
}