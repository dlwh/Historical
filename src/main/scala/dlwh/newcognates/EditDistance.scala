package dlwh.newcognates

import scalanlp.math.Numerics
import scalanlp.util._
import scalanlp.fst.fast.AutomatonFactory
import scalanlp.config.Configuration
import java.io.File
import scalanlp.fst.Alphabet
import scalanlp.concurrent.ParallelOps._
import scalala.tensor.adaptive.AdaptiveVector
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

object EditDistance {
  import math._;
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

  def fullMatrixCostMatrix(charIndex: Index[Char], countsMatrix: Vector) = {
    val pe = new AlignmentPairEncoder(charIndex)
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


  def editDistance(s1: String, s2: String, costs: (Char,Char)=>Double) = {
    val matrix = editMatrix(s1, s2, costs)

    logsum(matrix(s1.length)(s2.length)(NOEPS), matrix(s1.length)(s2.length)(LEFTEPS), matrix(s1.length)(s2.length)(RIGHTEPS));
  }

  // for expectedcounts


  def expectedCounts(charIndex: Index[Char], s1: String, s2: String, costs: (Char,Char)=>Double): AdaptiveVector = {
    val pe = new AlignmentPairEncoder(charIndex);
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

    result;

  }
}



object ComputeOracleWeights {

  import scalala.Scalala._;


  def expectedCountsToMatrix(oldCounts: ECounts, index: Index[Char]): (String, String) => (Char, Char) => Double = {
    val costMatrix = oldCounts.mapValues {
      counts =>
        val v = new DenseVector(counts.size);
        v += 1E-10;
        v += counts;
        EditDistance.fullMatrixCostMatrix(index, v)
    };
    (a: String, b: String) => costMatrix(a -> b);
  }

  def costMatrix(index: Index[Char], config: Configuration) = {
    val file = config.readIn[File]("oldcounts",null);
    if( file ne null) {
      val oldCounts: Map[(Language,Language),Vector] = readObject(file);
      expectedCountsToMatrix(oldCounts, index)
    } else {
      val subCost = config.readIn[Double]("initSub");
      val delCost = config.readIn[Double]("initDel");
      {(a:String,b:String)=>EditDistance.simpleCostMatrix(index.size-1,subCost, delCost)}
    }
  }


  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val legalGloss = args(0).split(':').map(Symbol(_)).toSet;
    println(legalGloss);
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    val deathScore = math.log(config.readIn[Double]("initDeathProb"));

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val basetree: Tree = dataset.tree;
    val tree = basetree.subtreeAt(config.readIn[String]("subtree",basetree.label));
    val leaves = tree.leaves;

    val cogs = dataset.cognates
    val alphabet = Index(cogs.iterator.flatMap(_.iterator).flatMap(_.word.iterator) ++ Iterator.single(implicitly[Alphabet[Char]].epsilon));
    println(alphabet);

    val filtered = cogs.filter(group => legalGloss(group.head.gloss))
    val gold: IndexedSeq[Seq[Cognate]] = filtered.map(_.filter(cog => leaves(cog.language))).filterNot(_.isEmpty);
    val goldTags = Accuracy.assignGoldTags(gold);
    val numGold = Accuracy.numGold(gold);


    import scalanlp.math.Semiring.LogSpace._;
    val autoFactory = new AutomatonFactory(alphabet);
    val factorFactory = new WordFactorsFactory(autoFactory)
    import factorFactory.factory.{EditDistance=> _, _};

    val costMatrixMaker = costMatrix(index,config);

    def deathProbs(a: Language, b: Language) = deathScore;

    val groupedByGloss: Map[Symbol, IndexedSeq[scala.Seq[Cognate]]] = gold.groupBy(_.head.gloss);

    val allEdges = tree.edges;

    val start: ECounts = (for( edge <- allEdges) yield {
      val startingScores = new SparseVector(index.size*index.size);
      /*
      for( i <- 0 until index.size; j <- 0 until index.size) {
        startingScores(EditDistance.encode(index,i,j)) = if (i == index('\0') && j == index('\0')) 0.0
        else if (i == j) .20
        else if(i == index('\0') || j == index('\0')) 0.003
        else 0.003;
      }
      */
      edge -> (startingScores:Vector)
    })toMap


    val bigScores = groupedByGloss.foldLeft(start) {  (acc,pair) =>
      val (gloss,groups) = pair
      val legalWords = groups.flatMap(_.map(_.word)).toSet;
      val factors = new factorFactory.WordFactors(legalWords, costMatrixMaker);


      val counts = groups.par(2).mapReduce[ECounts,ECounts]({ (group) =>
        groupToExpectedCounts(CognateGroup(group: _*), index, factors, tree, allEdges)
      }, sumCounts(_:ECounts,_:ECounts))
      sumCounts(acc,counts);
    }
    val pe = new AlignmentPairEncoder(index);
    println(bigScores.mapValues(pe.decode(_)));
    writeObject(new File("soundlaws.ser"),bigScores);
    writeObject(new File("index.ser"),index);
  }


  def groupToExpectedCounts(cg: CognateGroup,
                            charIndex: Index[Char],
                            factors: WordFactorsFactory#WordFactors,
                            tree: Tree,
                            allEdges: Set[(String, String)]):ECounts  = {
    val inf = new TreeElimination(factors, tree, cg);
    println(cg.prettyString(tree) + " has score " + inf.likelihood);
    val edgeCounts = (for (pair@(parent, child) <- allEdges.iterator;
                           marginal <- inf.edgeMarginal(parent, child).iterator)
    yield {pair -> marginal.expectedCounts});
    val result = edgeCounts.toMap;
    result
  }

  type ECounts = Map[(Language,Language),Vector]

  def sumCounts(s1: ECounts, s2: ECounts) = {
    val r = collection.mutable.Map[(Language,Language),Vector]();
    r ++= s1;
    for( (k,v) <- s2) {
      if(r.contains(k)) r(k) += v
      else r(k) = v;
    }
    r.toMap

  }

}
