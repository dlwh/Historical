package dlwh.newcognates

import scalanlp.math.Numerics
import scalanlp.util._
import scalanlp.fst.fast.AutomatonFactory
import scalanlp.config.Configuration
import java.io.File
import scalanlp.fst.Alphabet
import scalanlp.concurrent.ParallelOps._
import scalala.tensor.adaptive.AdaptiveVector
import scalala.Scalala._;
import scalala.tensor.Vector
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.sparse.SparseVector
import scalala.tensor.counters.Counters
;

/**
 * 
 * @author dlwh
 */

class ThreeStateEditDistance(val charIndex: Index[Char], subRatio: Double = -2, insRatio: Double= -2) extends EditDistance {
  import math._;

  protected val pe = new AlignmentPairEncoder(charIndex)

  def makeParameters(stats: Map[Language, SufficientStatistics]):Map[Language,Parameters] = {
    stats.mapValues(s => fullCostMatrix(s.counts));
  }

  case class SufficientStatistics(counts: Vector) extends BaseSufficientStatistics {
    def +(stats: SufficientStatistics) = SufficientStatistics(this.counts + stats.counts value);
    def *(weight: Double) = SufficientStatistics(this.counts * weight value);
  }
  type Parameters = (Char,Char)=>Double

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

    (c1: Char, c2: Char) => {
      if(c1 == c2) matchCost
      else if (c1 == '\0' || c2 == '\0') insCost;
      else subCost;
    }
  }

  def fullCostMatrix(countsMatrix: Vector) = {
    val decoded: PairedDoubleCounter[Char, Char] = pe.decode(countsMatrix);
    val epsIndex = charIndex('\0')
    val insertionMass = decoded('\0').total / decoded.total;
    val subDel = math.log(1 - insertionMass)
    val totals = Counters.marginalize(decoded);

    val result = new DenseVector(countsMatrix.size);
    var i = 0;
    while(i < result.size) {
      val (p,c) = pe.decode(i);
      if(p == epsIndex) {
        result(i) = math.log(countsMatrix(i)) - math.log(decoded.total);
        assert(!result(i).isNaN);
      } else {
        val denom = totals(charIndex.get(p));
        if(denom == 0) result(i) = Double.NegativeInfinity;
        else result(i) = math.log(countsMatrix(i)) - math.log(denom) + subDel
        assert(!result(i).isNaN,(denom,subDel,countsMatrix(i)));
      };
      i+=1
    }

    (c1: Char, c2: Char) => {
      result(pe.encode(charIndex(c1),charIndex(c2)));
    }
  }

  def logsum(a: Double, b: Double, c: Double) = {
    val max = a max b max c;
    if(max.isInfinite) max
    else {
      val accum = exp(a - max) + exp(b-  max) + exp(c - max);
      max + log(accum);
    }
  }

  val NOEPS = 0;
  val LEFTEPS = 1;
  val RIGHTEPS = 2;
  def editMatrix(s1: String, s2: String, costs: (Char, Char) => Double): Array[Array[Array[Double]]] = {
    val matrix = Array.fill(s1.length+1,s2.length+1,3)(Double.NegativeInfinity);

    var i = 0;
    while (i <= s1.length) {
      var j = 0;
      while (j <= s2.length) {
        if (i == 0 && j == 0) {
          matrix(i)(j)(NOEPS) = 0
        } else if (i == 0) {
          matrix(i)(j)(LEFTEPS) = costs('\0', s2(j - 1)) + Numerics.logSum(matrix(i)(j - 1)(LEFTEPS),matrix(i)(j-1)(NOEPS));
        } else if (j == 0) {
          matrix(i)(j)(RIGHTEPS) = costs(s1(i - 1), '\0') + Numerics.logSum(matrix(i-1)(j)(RIGHTEPS),matrix(i-1)(j)(NOEPS));
        } else {
          matrix(i)(j)(LEFTEPS) = Numerics.logSum(matrix(i)(j-1)(LEFTEPS), matrix(i)(j-1)(NOEPS)) + costs('\0',s2(j-1));
          matrix(i)(j)(RIGHTEPS) = logsum(matrix(i-1)(j)(LEFTEPS), matrix(i-1)(j)(NOEPS), matrix(i-1)(j)(RIGHTEPS)) + costs(s1(i-1),'\0');
          matrix(i)(j)(NOEPS) = logsum(matrix(i-1)(j-1)(RIGHTEPS), matrix(i-1)(j-1)(NOEPS), matrix(i-1)(j-1)(LEFTEPS)) + costs(s1(i-1),s2(j-1));
        };
        j += 1;
      }
      i += 1;
    }
    matrix
  }

  def backwardEditMatrix(s1: String, s2: String, costs: (Char, Char) => Double): Array[Array[Array[Double]]] = {
    val matrix = Array.fill(s1.length+1,s2.length+1,3)(Double.NegativeInfinity);

    var i = s1.length;
    while (i >= 0) {
      var j = s2.length;
      while (j >= 0) {
        if (i == s1.length && j == s2.length) {
          matrix(i)(j)(NOEPS) = 0
          matrix(i)(j)(LEFTEPS) = 0
          matrix(i)(j)(RIGHTEPS) = 0
        } else if (i == s1.length) {
          matrix(i)(j)(LEFTEPS) = costs('\0', s2(j)) + matrix(i)(j + 1)(LEFTEPS);
          matrix(i)(j)(NOEPS) = costs('\0', s2(j)) + matrix(i)(j + 1)(LEFTEPS);
        } else if (j == s2.length) {
          matrix(i)(j)(RIGHTEPS) = costs(s1(i), '\0') + matrix(i+1)(j)(RIGHTEPS);
          matrix(i)(j)(LEFTEPS) = costs(s1(i), '\0') + matrix(i+1)(j)(RIGHTEPS);
          matrix(i)(j)(NOEPS) = costs(s1(i), '\0') + matrix(i+1)(j)(RIGHTEPS);
        } else {
          matrix(i)(j)(LEFTEPS) = logsum(costs('\0',s2(j)) + matrix(i)(j+1)(LEFTEPS),
                                         costs(s1(i),s2(j)) + matrix(i+1)(j+1)(NOEPS),
                                         costs(s1(i),'\0') + matrix(i+1)(j)(RIGHTEPS));
          matrix(i)(j)(RIGHTEPS) = Numerics.logSum(costs(s1(i),s2(j)) + matrix(i+1)(j+1)(NOEPS),
                                                  matrix(i+1)(j)(RIGHTEPS) + costs(s1(i),'\0'));
          matrix(i)(j)(NOEPS) = logsum(costs(s1(i),'\0') + matrix(i+1)(j)(RIGHTEPS),
                                       costs(s1(i),s2(j)) + matrix(i+1)(j+1)(NOEPS),
                                       costs('\0',s2(j)) + matrix(i)(j+1)(LEFTEPS));
        };
        j -= 1;
      }
      i -= 1;
    }
    matrix
  }


  def distance(costs: Parameters, s1: String, s2: String) = {
    val matrix = editMatrix(s1, s2, costs)

    logsum(matrix(s1.length)(s2.length)(NOEPS),
      matrix(s1.length)(s2.length)(LEFTEPS),
      matrix(s1.length)(s2.length)(RIGHTEPS));
  }

  def sufficientStatistics(costs:Parameters, s1: String, s2: String): SufficientStatistics = {
    val forwardMatrix = editMatrix(s1,s2,costs);
    val reverseMatrix: Array[Array[Array[Double]]] = backwardEditMatrix(s1,s2,costs);
    val partition: Double = logsum(forwardMatrix(s1.length)(s2.length)(NOEPS), forwardMatrix(s1.length)(s2.length)(LEFTEPS), forwardMatrix(s1.length)(s2.length)(RIGHTEPS));
    val result = new AdaptiveVector(charIndex.size * charIndex.size);
    val epsIndex = charIndex('\0')
    val indexedS1 = s1.map(charIndex);
    val indexedS2 = s2.map(charIndex);
    var i = 0;

    while (i <= s1.length) {
      var j = 0;
      while (j <= s2.length) {
        import math.exp;

        if(i > 0 && j > 0) {
          val matchF: Double = forwardMatrix(i)(j)(NOEPS)
          val matchB: Double = reverseMatrix(i)(j)(NOEPS)
          val matchCount = exp(matchF+ matchB- partition)
          if(matchCount > 1E-7)
            result( pe.encode(indexedS1(i-1),indexedS2(j-1))) += matchCount;
        }

        if( j > 0) {
          val insF: Double = forwardMatrix(i)(j)(LEFTEPS)
          val insB: Double = reverseMatrix(i)(j)(LEFTEPS)
          val insCount = exp(insF+ insB- partition)
          if(insCount > 1E-7)
            result(pe.encode(epsIndex,indexedS2(j-1)))+= insCount;
        }

        if(i > 0) {
          val delF: Double = forwardMatrix(i)(j)(RIGHTEPS)
          val delB: Double = reverseMatrix(i)(j)(RIGHTEPS)
          val delCount = exp(delF+ delB- partition)
          result(pe.encode(indexedS1(i-1),epsIndex)) += delCount;
        }
        j += 1
      }
      i += 1
    }

    SufficientStatistics(result);

  }

}

