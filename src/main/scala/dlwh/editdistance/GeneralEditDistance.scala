package dlwh.editdistance

import dlwh.baldur._
import scalala.library.Numerics
import scalanlp.util._
import scalala.tensor._
import dense.{DenseVectorCol, DenseVector, DenseMatrix}
import java.util.Arrays
import scalanlp.optimize.{CachedDiffFunction, FirstOrderMinimizer}
import dlwh.editdistance.EditDistanceObjectiveFunction.{Feature,Insertion, MatchFeature, PairFeature, ChildFeature, LangFeature, StatefulFeature}
import sparse.SparseVector
import collection.mutable.ArrayBuffer

/**
 * 
 * @author dlwh
 */
class GeneralEditDistance(nStates:Int,
                          val charIndex: Index[Char],
                          subRatio: Int=>Double,
                          insRatio: Int=>Double,
                          transRatio: (Int,Int)=>Double) extends EditDistance {
  import math._;

  protected val pe = new AlignmentPairEncoder(charIndex)
  def pairEncoder = pe


  def featuresFor(l: Language, from:Int, to:Int, p: Char, c: Char): Array[Feature] = {
    val base = ArrayBuffer[Feature](PairFeature(p,c),ChildFeature(c));
    if(p == c) base += MatchFeature;
    val langFeats = base.map(LangFeature(l,_));
    base ++= langFeats
    (base ++ base.map(StatefulFeature(from, to, _))).toArray
  }

  def insertionFeaturesFor(l: Language, from: Int):Array[Feature] = {
    Array(LangFeature(l,Insertion),Insertion,StatefulFeature(from,from,Insertion),StatefulFeature(from,from,LangFeature(l,Insertion)));
  }

  var decodedParams:Counter[EditDistanceObjectiveFunction.Feature,Double] = null;
  def makeParameters(stats: Map[Language, SufficientStatistics]):Map[Language,Parameters] = {
    val nicer = stats.mapValues(_.counts);
    val obj = new EditDistanceObjectiveFunction(pe,nicer, featuresFor _, insertionFeaturesFor _, nStates);

    val opt = FirstOrderMinimizer.OptParams(useStochastic = false, maxIterations = 100, regularization = 2).minimizer(obj);
    val params = opt.minimize(new CachedDiffFunction(obj), if(decodedParams eq null) obj.initialWeightVector else obj.featureEncoder.encodeDense(decodedParams));
    decodedParams = obj.featureEncoder.decode(params);
    val theMap = stats.map { case (child,_) =>
      val matrices = Array.tabulate(nStates) { from =>
        val matrix = obj.costMatrixFor(child,from,params);
        matrix
      }
      (child)-> new Parameters {
        def initialStateWeight(s: Int) = if(s == 0) 0.0 else Double.NegativeInfinity

        def finalCost(s: Int) = 0.0

        def apply(s: Int, t: Int, c1: Int, c2: Int) = matrices(s)(t)(c1)(c2);
      }
    }
    theMap
  }

  case class SufficientStatistics(counts: Vector[Double]) extends BaseSufficientStatistics {
    def +(stats: SufficientStatistics) = {
      println(this.counts.norm(1),stats.counts.norm(1),(this.counts + stats.counts).norm(1))
      SufficientStatistics(this.counts + stats.counts)
    };
    def *(weight: Double) = SufficientStatistics(this.counts * weight);

    def decode = {
      val result = Array.fill(nStates,nStates) { DenseVector.zeros[Double](charIndex.size*charIndex.size)}
      for( (i,v) <- counts.pairsIteratorNonZero) {
        val begin = i % nStates
        val end = (i / nStates)%nStates
//        val (a,b) = pe.decode(i/nStates/nStates)
        result(begin)(end)(i/nStates/nStates) = v
      }
      result
    }
  }

  trait Parameters {
    def apply(s: Int, t: Int, c1: Int, c2: Int):Double
    def finalCost(s: Int):Double
    def initialStateWeight(s: Int):Double;
  }

  def emptySufficientStatistics = new SufficientStatistics(SparseVector.zeros[Double](nStates * nStates * charIndex.size * charIndex.size))

  def initialParameters = simpleCostMatrix(charIndex.size,subRatio,insRatio, transRatio)

  def simpleCostMatrix(numChars: Int, subRatio: Int=>Double, insRatio: Int=>Double, transRatio: (Int,Int)=>Double) = {
    val (insCost,subCost,matchCost) = {
      val n = numChars;
      import math.{exp,log};
      // for any input label (resp. output label), there is 1 match, n-1 subs, and and 1 deletion
      // but we also have to accept an insertion of any kind, so there are n of those.
      //  c * ((n - 1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 1.0
      // log c + log ((n -1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 0.0
      val r = (Array.tabulate(nStates){state =>
        val logC = - log( (n-1) * exp(subRatio(state)) + 1 + (n+1) * exp(insRatio(state)))
        (insRatio(state) + logC,subRatio(state) + logC,logC)
      })
      (r.map(_._1),r.map(_._2),r.map(_._3));
    }

    val transCost = {
      val mat = Array.tabulate(nStates,nStates) { transRatio } map { new DenseVectorCol(_)};
      for(row <- mat) {
        row -= Numerics.logSum(row.data);
      }
      mat
    }

    new Parameters {
      def apply(s: Int, t: Int, c1: Int, c2: Int) = {
        val trans = transCost(s)(t);
        if(c1 == c2) matchCost(s) + trans
        else if (c1 == pe.epsIndex || c2 == pe.epsIndex) insCost(s) + trans
        else subCost(s) + trans
      }

      def finalCost(s: Int) = math.log(1 - math.exp(math.log(numChars) + insCost(s)));

      def initialStateWeight(s: Int) = if(s == 0) 0.0 else Double.NegativeInfinity

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
  def editMatrix(ss1: String, ss2: String, costs: Parameters): Array[Array[DenseMatrix[Double]]] = {
    import costs._;
    val s1 = ss1.map(charIndex);
    val s2 = ss2.map(charIndex);
    val matrix = Array.fill(nStates,3)(DenseMatrix.zeros[Double](s1.length + 1,s2.length+ 1));
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

  def backwardEditMatrix(ss1: String, ss2: String, costs: Parameters) = {
    import costs._;
    val s1 = ss1.map(charIndex);
    val s2 = ss2.map(charIndex);
    val matrix = Array.fill(nStates,3)(DenseMatrix.zeros[Double](s1.length + 1,s2.length+ 1));
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


  def partition(costs: GeneralEditDistance.this.type#Parameters, matrix: Array[Array[DenseMatrix[Double]]], s1: String, s2: String): Double = {
    val scratch = new Array[Double](nStates * 3);
    var state = 0;
    while (state < nStates) {
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

  def encodeTransition(state: Int, nextState: Int, x: Int, y: Int): Int = {
    state + nStates * (nextState + nStates * pe.encode(x, y))
  }

  def sufficientStatistics(costs:Parameters, s1: String, s2: String): SufficientStatistics = {
    val forwardMatrix = editMatrix(s1,s2,costs);
    val reverseMatrix = backwardEditMatrix(s1,s2,costs);
    val partition = this.partition(costs,forwardMatrix,s1,s2);
    val result = DenseVector.zeros[Double](nStates * nStates * charIndex.size * charIndex.size);
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
              if(matchCount > 1E-7) {
                val x = indexedS1(i - 1)
                val y = indexedS2(j - 1)
                result(encodeTransition(state, nextState, x, y)) += matchCount
              };
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
                result(encodeTransition(state,nextState,epsIndex,indexedS2(j-1)))+= insCount;
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
                result(encodeTransition(state, nextState,  indexedS1(i-1),epsIndex))+= insCount;
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

