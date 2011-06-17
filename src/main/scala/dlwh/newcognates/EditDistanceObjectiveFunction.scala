package dlwh.newcognates

import scalala.tensor.dense.DenseVector
import scalala.tensor.Vector
import scalanlp.util._;
import scalanlp.math.Numerics
import collection.mutable.ArrayBuffer
import scalanlp.config.Configuration
import java.io.File
import scalala.Scalala._
import scalala.tensor.counters.Counters._;
import scalanlp.optimize._
import scalanlp.concurrent.ParallelOps._

trait FeaturizedOptimization { this: ThreeStateEditDistance =>
  var decodedParams:DoubleCounter[EditDistanceObjectiveFunction.Feature] = null;
  override def makeParameters(stats: Map[Language, SufficientStatistics]):Map[Language,Parameters] = {
    val nicer = stats.mapValues(_.counts);
    val obj = new EditDistanceObjectiveFunction(pe,nicer, EditDistanceObjectiveFunction.featuresFor _, EditDistanceObjectiveFunction.insertionFeaturesFor _);

    val opt = FirstOrderMinimizer.OptParams(useStochastic = false, maxIterations = 100, regularization = 2).minimizer(obj);
    val params = opt.minimize(new CachedDiffFunction(obj), if(decodedParams eq null) obj.initialWeightVector else obj.featureEncoder.encodeDense(decodedParams));
    decodedParams = obj.featureEncoder.decode(params);
    val theMap = stats.map { case (child,_) =>
      val matrix = obj.costMatrixFor(child,0,params);
      def cost(c1: Char, c2: Char) = matrix(0)(charIndex(c1))(charIndex(c2));
      (child)-> (cost _);
    }
    theMap
  }
}


/**
 * 
 * @author dlwh
 **/
class EditDistanceObjectiveFunction[F:ClassManifest](pe: AlignmentPairEncoder, expectedCounts: Map[Language,Vector],
                                    featurizer: (Language,Int,Int,Char,Char)=>Array[F],
                                    insertionFeaturizer: (Language,Int)=>Array[F],
                                    nStates:Int = 1) extends DiffFunction[Int,DenseVector] {
  val (featuresIndex: Index[F],
       featuresForPair: Map[Language,Array[Array[Int]]],// features are (from,to,p,c)
       insertionFeatures: Map[Language,Array[Array[Int]]]) = { // Language->from->insretion features
    val ci: Index[Char] = pe.charIndex;
    val featureIndex = Index[F]();
    val feats = expectedCounts.map { case (lang,ecounts) =>
      assert(ecounts.size == nStates * nStates * pe.size)
      lang -> Array.tabulate(ecounts.size) { pair =>
        if(pair == pe.doubleEpsilon) Array.empty[Int]
        else {
          val (ci(p),ci(c)) = pe.decode(pair/nStates/nStates);
          val from = pair%nStates;
          val to = (pair/nStates)%nStates;
          val features = featurizer(lang,from,to,p,c).map(featureIndex.index _);
          features;
        }
      }
    }

    val insertionFeatures = expectedCounts.keys.map { language =>
      language -> Array.tabulate(nStates) { from => insertionFeaturizer(language,from).map(featureIndex.index _)}
    }.toMap;

    (featureIndex:Index[F],feats, insertionFeatures:Map[Language,Array[Array[Int]]]);
  }

  val featureEncoder = Encoder.fromIndex(featuresIndex);
  def initialWeightVector = featureEncoder.mkDenseVector(0.0);

  // to -> p -> c
  def computeLogProbs(lang: Language,from:Int, featureWeights: DenseVector): (Double, Array[Array[Array[Double]]]) = {
    val featuresArray = featuresForPair(lang);
    val insertionScore = score(insertionFeatures(lang)(from),featureWeights);
    val logProbInsertion = math.log(Numerics.sigmoid(insertionScore));

    val unnormalized = Array.tabulate(pe.charIndex.size,nStates,pe.charIndex.size) { (p,to,c) =>
      val pair = pe.encode(p,c);
      if(pair != pe.doubleEpsilon) {
        val encodedIndex = from + nStates * (to + pair * nStates);
        val myScore = score(featuresArray(encodedIndex),featureWeights);
        assert(!myScore.isInfinite);
        myScore;
      } else Double.NegativeInfinity;
    }
    val partitions = unnormalized.map(tocMatrix => Numerics.logSum(tocMatrix.map(Numerics.logSum _)));
    val normalized = Array.tabulate(nStates,pe.charIndex.size,pe.charIndex.size) { (to,p,c)=>
      unnormalized(p)(to)(c) - partitions(p)
    }
    (logProbInsertion,normalized);

  }

  /** result is (to)(p)(c)=>Double, as a cost */
  def costMatrixFor(lang: Language, from:Int, featureWeights: DenseVector) = { 
    val (logProbInsertion:Double,alignmentLogProbs) = computeLogProbs(lang,from,featureWeights);
    val logProbNotInsertion = math.log(1.0- math.exp(logProbInsertion));
    Array.tabulate(nStates,pe.charIndex.size,pe.charIndex.size) { (to,p,c) =>
      if(p == '\0' && c == '\0') Double.NegativeInfinity
      else if(p == pe.epsIndex) alignmentLogProbs(to)(p)(c) + logProbInsertion;
      else alignmentLogProbs(to)(p)(c) + logProbNotInsertion;
    }
  }


  // feats dot weights
  def score(feats: Array[Int], weights: DenseVector) = {
    var result = 0.0;
    var fi = 0;
    while(fi < feats.length) {
      result += weights(feats(fi));
      fi += 1;
    }
    result;
  }


  def weightedSum(deriv: DenseVector, features:Array[Int], weight: Double) ={
    var fi = 0;
    while(fi < features.length) {
      deriv(features(fi)) += weight;
      fi+=1;
    }
  }

  def calculate(featureWeights: DenseVector) = {
    def mkderiv = featureWeights.like;
    val (logProb,deriv) = expectedCounts.toIndexedSeq.par(2).fold( (0.,mkderiv)) { (gradObj,pair) =>
      var logProb = gradObj._1
      val deriv = gradObj._2
      val (lang,ecounts) = pair
      for(from <- 0 until nStates) {
        val featuresForPair:Array[Array[Int]] = this.featuresForPair(lang);
        // alignmentLogProbs: (p)(to)(c)->p(to,c|from,p;lang)
        val (logProbInsertion:Double,alignmentLogProbs: Array[Array[Array[Double]]]) = computeLogProbs(lang,from,featureWeights);
        //      println(lang -> pe.decode(new DenseVector(alignmentLogProbs)));
        //      println("ecounts" -> pe.decode(ecounts));
        var insertionCounts = 0.;
        var nonInsertionCounts = 0.;
        var p = 0;
        while(p < pe.charIndex.size) {
          var to = 0;
          while(to < nStates) {
            var c = 0;
            while(c < pe.charIndex.size) {
              val pair = from + nStates * (to + nStates * pe.encode(p,c));
              if( (c != pe.epsIndex || p != pe.epsIndex) && ecounts(pair) != 0.0 ) {
                logProb += ecounts(pair) * alignmentLogProbs(to)(p)(c);
                // need this for pInsertion
                if(p != pe.epsIndex) {
                  nonInsertionCounts += ecounts(pair)
                } else {
                  insertionCounts += ecounts(pair)
                }

                weightedSum(deriv,featuresForPair(pair),-ecounts(pair))

                // normalizer:
                var cPrime = 0;
                while(cPrime < pe.charIndex.size) {
                  val pairPrime = pe.encode(p,cPrime);
                  if(pairPrime != pe.doubleEpsilon) {
                    var toPrime = 0;
                    while(toPrime < nStates) {
                      val tuplePrime = from + nStates * (toPrime + nStates * pairPrime)
                      weightedSum(deriv,featuresForPair(tuplePrime),ecounts(pair) * math.exp(alignmentLogProbs(toPrime)(p)(cPrime)));
                      toPrime += 1;
                    }
                  }
                  cPrime += 1;
                }
              }
              c+=1;
            }
            to += 1;
          }
          p += 1;
        }
        assert(!logProb.isNaN);
        // ll = insertionCounts * log p(insert) + nonInsertionCounts * log p(not Insert)
        if(insertionCounts != 0.0)
          logProb += insertionCounts * logProbInsertion;
        if(nonInsertionCounts != 0.0)
          logProb += nonInsertionCounts * math.log(1-math.exp(logProbInsertion));
        val yesGradPart = insertionCounts * (1 - math.exp(logProbInsertion));
        val noGradPart = -(nonInsertionCounts) * math.exp(logProbInsertion)
        val gradContribution = yesGradPart + noGradPart;
        weightedSum(deriv,insertionFeatures(lang)(from),-gradContribution);
        assert(!logProb.isNaN,insertionCounts + " " + logProbInsertion + " " + nonInsertionCounts + " " + math.log(1-math.exp(logProbInsertion)));
      }
      (logProb,deriv)
    } { (a,b) =>
      a._2 += b._2
      (a._1 + b._1, a._2)
    }
    println(logProb,norm(deriv,2));
    (-logProb,deriv)
  }
}

object EditDistanceObjectiveFunction {
  class Feature
  case object Insertion extends Feature;
  case class LangFeature(l: Language, f: Feature) extends Feature;
  case class PairFeature(p : Char, c: Char) extends Feature;
  case class ChildFeature(c: Char) extends Feature;
  case object MatchFeature extends Feature;
  case class StatefulFeature(from: Int, to: Int, f: Feature) extends Feature;

  def featuresFor(l: Language, from:Int, to:Int, p: Char, c: Char): Array[Feature] = {
    val base = ArrayBuffer[Feature](PairFeature(p,c),ChildFeature(c));
    if(p == c) base += MatchFeature;
    base ++= base.map(LangFeature(l,_));
    base.toArray
  }

  def insertionFeaturesFor(l: Language, from: Int):Array[Feature] = {
    Array(LangFeature(l,Insertion),Insertion);
  }

  def optimize(index: Index[Char], expectedCounts: Map[(Language,Language),Vector]) = {
    val nicer = expectedCounts.map { case (k,v) => k._2 -> v};
    val pe = new AlignmentPairEncoder(index);
    val obj = new EditDistanceObjectiveFunction(pe,nicer,featuresFor _, insertionFeaturesFor _ );

    val opt = FirstOrderMinimizer.OptParams(useStochastic = false, maxIterations = 50, regularization = 2).minimizer(obj);
    val params = opt.minimize(new CachedDiffFunction(obj), obj.initialWeightVector);
//    println(obj.featureEncoder.decode(params));
    val theMap = expectedCounts.map { case ((parent,child),_) =>
      val matrix = obj.costMatrixFor(child,0,params);
      def cost(c1: Char, c2: Char) = matrix(0)(index(c1))(index(c2));
      (child)-> (cost _);
    }

    def costsFor(parent: Language,child:Language) = theMap(child);
    costsFor _;
  }
}

object RunEditDistanceObjectiveFunction {
  import EditDistanceObjectiveFunction._

  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    val deathScore = math.log(config.readIn[Double]("initDeathProb"));

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new ResourceDataset(dataset_name,languages, withGloss);
    val basetree: Tree = dataset.tree;
    val tree = basetree.subtreeAt(config.readIn[String]("subtree",basetree.label));
    val leaves = tree.leaves;
    val file = config.readIn[File]("oldcounts");
    val oldCounts: Map[(Language,Language),Vector] = readObject(file);
    val nicer = oldCounts.map { case (k,v) => k._2 -> v};
    val index: Index[Char] = readObject(new File("index.ser"));
    val pe = new AlignmentPairEncoder(index);
    val obj = new EditDistanceObjectiveFunction(pe,nicer,featuresFor _, insertionFeaturesFor _ );

    val opt = FirstOrderMinimizer.OptParams(useStochastic = false, maxIterations = 50).minimizer(obj);
    val params = opt.minimize(obj, obj.initialWeightVector);
    println(obj.featureEncoder.decode(params));
  }
}
