package dlwh.newcognates

import scalanlp.math.Numerics
import scalanlp.util._
import scalala.tensor.adaptive.AdaptiveVector
import scalala.Scalala._;
import scalala.tensor.Vector
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.Counters
import java.util.Arrays
import sun.font.StandardTextSource

/**
 * 
 * @author dlwh
 */
class GeneralEditDistance(val charIndex: Index[Char], subRatio: Double = -3, insRatio: Double= -3) extends EditDistance {
  import math._;

  protected val pe = new AlignmentPairEncoder(charIndex)

  def makeParameters(stats: Map[Language, SufficientStatistics]):Map[Language,Parameters] = {
    TODO
  }

  case class SufficientStatistics(counts: Vector) extends BaseSufficientStatistics {
    def +(stats: SufficientStatistics) = SufficientStatistics(this.counts + stats.counts value);
    def *(weight: Double) = SufficientStatistics(this.counts * weight value);
  }

  trait Parameters {
    def apply(s: Int, t: Int, c1: Char, c2: Char):Double
    def finalCost(s: Int):Double
    def nStates:Int;
    def initialStateWeight(s: Int):Double;
  }

  def emptySufficientStatistics = new SufficientStatistics(new AdaptiveVector(charIndex.size * charIndex.size))

  def initialParameters = simpleCostMatrix(charIndex.size,subRatio,insRatio)

  def simpleCostMatrix(numChars: Int, subRatio: Double, insRatio: Double) = {
    val (insCost,subCost,matchCost) = {
      val n = numChars;
      import math.{exp,log};
      // for any input label (resp. output label), there is 1 match, n-1 subs, and and 1 deletion
      // but we also have to accept an insertion of any kind, so there are n of those.
      //  c * ((n - 1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 1.0
      // log c + log ((n -1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 0.0
      val logC = - log( (n-1) * exp(subRatio) + 1 + (n+1) * exp(insRatio))
      (insRatio + logC,subRatio + logC,logC)
    }

    new Parameters {
      def apply(s: Int, t: Int, c1: Char, c2: Char) = {
        if(t != s) Double.NegativeInfinity
        else if(c1 == c2) matchCost
        else if (c1 == '\0' || c2 == '\0') insCost;
        else subCost;
      }

      def finalCost(s: Int) = 0.0  //math.log(1 - math.exp(math.log(totalChars) + insCost));
      val nStates = 1

      def initialStateWeight(s: Int) = 0.0
    }


  }

  final def logsum(a: Double, b: Double, c: Double) = {
    val max = a max b max c;
    if(max.isInfinite) max
    else {
      val accum = exp(a - max) + exp(b-  max) + exp(c - max);
      max + log(accum);
    }
  }

  final def logsum(a: Double, b: Double, c: Double, d: Double) = {
    val max = a max b max c max d;
    if(max.isInfinite) max
    else {
      val accum = exp(a - max) + exp(b-  max) + exp(c - max) + exp(d-max);
      max + log(accum);
    }
  }

  val NOEPS = 0;
  val LEFTEPS = 1;
  val RIGHTEPS = 2;
  def editMatrix(s1: String, s2: String, costs: Parameters): Array[Array[DenseMatrix]] = {
    import costs._;
    val nStates = costs.nStates;
    val matrix: Array[Array[DenseMatrix]] = Array.fill(nStates,3)(new DenseMatrix(s1.length + 1,s2.length+ 1));
    //Array.fill(s1.length+1,s2.length+1,3)(Double.NegativeInfinity);
    var state = 0;
    var prevState = 0;
    while(state < nStates) {
      Arrays.fill(matrix(state)(0).data,Double.NegativeInfinity);
      Arrays.fill(matrix(state)(1).data,Double.NegativeInfinity);
      Arrays.fill(matrix(state)(2).data,Double.NegativeInfinity);
      state += 1;
    }

    val scratch = new Array[Double](nStates * 3);

    var i = 0;
    while (i <= s1.length) {
      var j = 0;
      while (j <= s2.length) {
        if (i == 0 && j == 0) {
          state = 0;
          while(state < nStates) {
            matrix(state)(NOEPS)(0,0) = initialStateWeight(state);
            state += 1;
          }
        } else {
          if (j != 0) { // insertions
            state = 0;
            while(state < nStates) {
              prevState = 0;
              while(prevState < nStates) {
                scratch(prevState*2) = costs(prevState,state,'\0',s2(j-1)) + matrix(prevState)(LEFTEPS)(i,j-1)
                scratch(prevState*2+1) = costs(prevState,state,'\0',s2(j-1)) +matrix(prevState)(NOEPS)(i,j-1);
                prevState += 1;
              }
              matrix(state)(LEFTEPS)(i,j) = Numerics.logSum(scratch,nStates*2);
              state += 1;
            }
          }
          if (i != 0) { // deletions
            state = 0;
            while(state < nStates) {
              prevState = 0;
              while(prevState < nStates) {
                scratch(prevState*3) = costs(prevState,state,s1(i-1),'\0') + matrix(prevState)(RIGHTEPS)(i-1,j);
                scratch(prevState*3+1) = costs(prevState,state,s1(i-1),'\0') + matrix(prevState)(NOEPS)(i-1,j);
                if(j != 0)
                  scratch(prevState*3+2) = costs(prevState,state,s1(i-1),'\0') + matrix(prevState)(LEFTEPS)(i-1,j);
                else
                  scratch(prevState*3 + 2) = Double.NegativeInfinity
                prevState += 1;
              }
              matrix(state)(RIGHTEPS)(i,j) = Numerics.logSum(scratch,nStates*3);
              state += 1;
            }
          }
          if(i != 0 && j != 0) { // matches
            state = 0;
            while(state < nStates) {
              prevState = 0;
              while(prevState < nStates) {
                scratch(prevState*3) = costs(prevState,state,s1(i-1),s2(j-1)) + matrix(prevState)(RIGHTEPS)(i-1,j-1);
                scratch(prevState*3+1) = costs(prevState,state,s1(i-1),s2(j-1)) + matrix(prevState)(NOEPS)(i-1,j-1);
                scratch(prevState*3+2) = costs(prevState,state,s1(i-1),s2(j-1)) + matrix(prevState)(LEFTEPS)(i-1,j-1);
                prevState += 1;
              }
              matrix(state)(NOEPS)(i,j) = Numerics.logSum(scratch,nStates*3);
              state += 1;
            }
          }
        };
        j += 1;
      }
      i += 1;
    }
    (matrix)
  }

  def backwardEditMatrix(s1: String, s2: String, costs: Parameters) = {
    import costs._;
    val nStates = costs.nStates;
    val matrix: Array[Array[DenseMatrix]] = Array.fill(nStates,3)(new DenseMatrix(s1.length + 1,s2.length+ 1));
    //Array.fill(s1.length+1,s2.length+1,3)(Double.NegativeInfinity);
    var state = 0;
    var nextState = 0;
    while(state < nStates) {
      Arrays.fill(matrix(state)(0).data,Double.NegativeInfinity);
      Arrays.fill(matrix(state)(1).data,Double.NegativeInfinity);
      Arrays.fill(matrix(state)(2).data,Double.NegativeInfinity);
      state += 1;
    }

    val scratch = new Array[Double](nStates * 3);

    var i = s1.length;
    while (i >= 0) {
      var j = s2.length;
      while (j >= 0) {
        if (i == s1.length && j == s2.length) {
          state = 0
          while(state < nStates) {
            val finalCost = costs.finalCost(state)
            matrix(state)(NOEPS)(i,j) = finalCost;
            matrix(state)(LEFTEPS)(i,j) = finalCost;
            matrix(state)(RIGHTEPS)(i,j) = finalCost;
            state += 1
          }
        } else {
          // insertions
          // TODO: this could be much faster
          Arrays.fill(scratch,Double.NegativeInfinity)
          state = 0;
          while(state < nStates) {
            nextState = 0;
            while(nextState < nStates) {
              if(j < s2.length)
                scratch(nextState*3) = costs(state,nextState,'\0',s2(j)) + matrix(nextState)(LEFTEPS)(i,j+1)
              if(j < s2.length && i < s1.length)
                scratch(nextState*3+1) = costs(state,nextState,s1(i),s2(j)) + matrix(nextState)(NOEPS)(i+1,j+1)
              if(i < s1.length)
                scratch(nextState*3+2) = costs(state,nextState,s1(i),'\0') + matrix(nextState)(RIGHTEPS)(i+1,j)
              nextState += 1;
            }
            matrix(state)(LEFTEPS)(i,j) = Numerics.logSum(scratch,nStates*3);
            state += 1;
          }
          // deletions
          // TODO: this could be much faster
          Arrays.fill(scratch,Double.NegativeInfinity)
          state = 0;
          while(state < nStates) {
            nextState = 0;
            while(nextState < nStates) {
              if(j < s2.length && i < s1.length)
                scratch(nextState*2) = costs(state,nextState,s1(i),s2(j)) + matrix(nextState)(NOEPS)(i+1,j+1)
              if(i < s1.length)
                scratch(nextState*2+1) = costs(state,nextState,s1(i),'\0') + matrix(nextState)(RIGHTEPS)(i+1,j)
              nextState += 1;
            }
            matrix(state)(RIGHTEPS)(i,j) = Numerics.logSum(scratch,nStates*2);
            state += 1;
          }
          // matches. The forward transitions for NOEPS is identical to LEFTEPS
          state = 0;
          while(state < nStates) {
            matrix(state)(NOEPS)(i,j) = matrix(state)(LEFTEPS)(i,j)
            state += 1;
          }
        }
        j -= 1;
      }
      i -= 1;
    }
    matrix
  }


  def partition(costs: GeneralEditDistance.this.type#Parameters, matrix: Array[Array[DenseMatrix]], s1: String, s2: String): Double = {
    val scratch = new Array[Double](costs.nStates * 3);
    var state = 0;
    while (state < costs.nStates) {
      val finalCost = costs.finalCost(state)
      scratch(state * 3) = matrix(state)(NOEPS)(s1.length, s2.length) + finalCost;
      scratch(state * 3 + 1) = matrix(state)(LEFTEPS)(s1.length, s2.length) + finalCost;
      scratch(state * 3 + 2) = matrix(state)(RIGHTEPS)(s1.length, s2.length) + finalCost;
      state += 1
    }

    Numerics.logSum(scratch);
  }

  def distance(costs: Parameters, s1: String, s2: String) = {
    val matrix = editMatrix(s1, s2, costs)
    partition(costs, matrix, s1, s2)
  }

  def sufficientStatistics(costs:Parameters, s1: String, s2: String): SufficientStatistics = {
    val nStates = costs.nStates;
    val forwardMatrix = editMatrix(s1,s2,costs);
    val reverseMatrix = backwardEditMatrix(s1,s2,costs);
    val partition = this.partition(costs,forwardMatrix,s1,s2);
    val result = new AdaptiveVector(nStates * nStates * charIndex.size * charIndex.size);
    val epsIndex = charIndex('\0')
    val indexedS1 = s1.map(charIndex);
    val indexedS2 = s2.map(charIndex);
    var i = 0;
    while (i <= s1.length) {
      var j = 0;
      while (j <= s2.length) {
        import math.exp;

        if(i > 0 && j > 0) {
          var state = 0;
          var nextState = 0;
          while(state < nStates) {
            while(nextState < nStates) {
              val matchF: Double = forwardMatrix(state)(NOEPS)(i,j)
              val matchB: Double = reverseMatrix(nextState)(NOEPS)(i,j)
              val matchCount = exp(matchF+ matchB- partition)
              if(matchCount > 1E-7)
                result(state + nStates * (nextState + nStates*pe.encode(indexedS1(i-1),indexedS2(j-1)))) += matchCount;
              nextState += 1;
            }
            state += 1;
          }
        }

        if( j > 0) {
          var state = 0;
          var nextState = 0;
          while(state < nStates) {
            while(nextState < nStates) {
              val matchF: Double = forwardMatrix(state)(LEFTEPS)(i,j)
              val matchB: Double = reverseMatrix(nextState)(LEFTEPS)(i,j)
              val insCount = exp(matchF+ matchB- partition)
              if(insCount > 1E-7)
                result(state + nStates * (nextState + nStates*pe.encode(epsIndex,indexedS2(j-1))))+= insCount;
              nextState += 1;
            }
            state += 1;
          }
        }

        if(i > 0) {
          var state = 0;
          var nextState = 0;
          while(state < nStates) {
            while(nextState < nStates) {
              val matchF: Double = forwardMatrix(state)(RIGHTEPS)(i,j)
              val matchB: Double = reverseMatrix(nextState)(RIGHTEPS)(i,j)
              val insCount = exp(matchF+ matchB- partition)
              if(insCount > 1E-7)
                result(state + nStates * (nextState + nStates*pe.encode(indexedS1(i-1),epsIndex)))+= insCount;
              nextState += 1;
            }
            state += 1;
          }
        }
        j += 1
      }
      i += 1
    }

    SufficientStatistics(result);

  }

}
