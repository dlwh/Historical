package dlwh.newcognates

import scalala.tensor.dense.DenseVector
import scalala.tensor.Vector
import scalanlp.util._;
import scalanlp.math.Numerics
import collection.mutable.ArrayBuffer
import scalanlp.config.Configuration
import java.io.File
import scalala.Scalala._
import scalanlp.optimize.{CachedDiffFunction, FirstOrderMinimizer, DiffFunction}
import scalanlp.maxent.MaxEntObjectiveFunction

trait FeaturizedOptimization { this: ThreeStateEditDistance =>
  override def makeParameters(stats: Map[Language, SufficientStatistics]):Map[Language,Parameters] = {
    val nicer = stats.mapValues(_.counts);
    val obj = new EditDistanceObjectiveFunction(pe,nicer, EditDistanceObjectiveFunction.featuresFor _, EditDistanceObjectiveFunction.insertionFeaturesFor _ );

    val opt = FirstOrderMinimizer.OptParams(useStochastic = false, maxIterations = 50, regularization = 2).minimizer(obj);
    val params = opt.minimize(new CachedDiffFunction(obj), obj.initialWeightVector);
    val theMap = stats.map { case (child,_) =>
      val matrix = obj.costMatrixFor(child,params);
      def cost(c1: Char, c2: Char) = matrix(charIndex(c1))(charIndex(c2));
      (child)-> (cost _);
    }
    theMap
  }
}


/**
 * 
 * @author dlwh
 **/
class EditDistanceObjectiveFunction[F](pe: AlignmentPairEncoder, expectedCounts: Map[Language,Vector],
                                    featurizer: (Language,Char,Char)=>Array[F],
                                    insertionFeaturizer: (Language)=>Array[F]) extends DiffFunction[Int,DenseVector] {
  val (featuresIndex: Index[F],
       featuresForPair: Map[Language,Array[Array[Int]]],
       insertionFeatures:Map[Language,Array[Int]]) = {
    val ci: Index[Char] = pe.charIndex;
    val featureIndex = Index[F];
    val feats = expectedCounts.map { case (lang,ecounts) =>
      lang -> Array.tabulate(ecounts.size) { pair =>
        if(pair == pe.doubleEpsilon) Array.empty[Int]
        else {
          val (ci(p),ci(c)) = pe.decode(pair);
          val features = featurizer(lang,p,c).map(featureIndex.index _);
          features;
        }
      }
    }

    val insFeats = expectedCounts.map { case (lang,_) => lang -> insertionFeaturizer(lang).map(featureIndex.index _)};
    (featureIndex:Index[F],feats,insFeats);
  }

  val featureEncoder = Encoder.fromIndex(featuresIndex);
  def initialWeightVector = featureEncoder.mkDenseVector(0.0);

  def computeLogProbs(lang: Language,featureWeights: DenseVector) = {
    val featuresArray = featuresForPair(lang);
    val insertionScore = score(insertionFeatures(lang),featureWeights);
    val logProbInsertion = insertionScore - Numerics.logSum(0.0,math.exp(insertionScore));
    val unnormalized = Array.tabulate(pe.charIndex.size,pe.charIndex.size) { (p,c) =>
      val pair = pe.encode(p,c);
      if(pair != pe.doubleEpsilon) {
        val myScore = score(featuresArray(pair),featureWeights);
        assert(!myScore.isInfinite);
        myScore;
      } else Double.NegativeInfinity;
    }
    val partitions = unnormalized.map(Numerics.logSum _);
    val normalized = Array.tabulate(expectedCounts(lang).size) { (pair)=>
      val (p,c) = pe.decode(pair);
      unnormalized(p)(c) - partitions(p)
    }
    (logProbInsertion,normalized);

  }

  def costMatrixFor(lang: Language, featureWeights: DenseVector) = {
    val (logProbInsertion:Double,alignmentLogProbs:Array[Double]) = computeLogProbs(lang,featureWeights);
    val logProbNotInsertion = math.log(1.0- math.exp(logProbInsertion));
    Array.tabulate(pe.charIndex.size,pe.charIndex.size) { (p,c) =>
      val pair = pe.encode(p,c);
      if(pair ==pe.doubleEpsilon) Double.NegativeInfinity
      else if(p == pe.epsIndex) alignmentLogProbs(pair) + logProbInsertion;
      else alignmentLogProbs(pair) + logProbNotInsertion;
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
    for( f <- features) {
      deriv(f) += weight;
    }
  }

  def calculate(featureWeights: DenseVector) = {
    var logProb = 0.;
    val deriv = featureWeights.like;
    for( (lang,ecounts) <- expectedCounts) {
      val featuresForPair = this.featuresForPair(lang);
      val (logProbInsertion:Double,alignmentLogProbs:Array[Double]) = computeLogProbs(lang,featureWeights);
//      println(lang -> pe.decode(new DenseVector(alignmentLogProbs)));
//      println("ecounts" -> pe.decode(ecounts));
      var pair = 0;
      var insertionCounts = 0.;
      var nonInsertionCounts = 0.;
      while( pair < alignmentLogProbs.length) {
        if(pair != pe.doubleEpsilon && ecounts(pair) != 0.0) {
          logProb += ecounts(pair) * alignmentLogProbs(pair);
          if(logProb.isNaN) {
            val (pe.charIndex(p),pe.charIndex(c)) = pe.decode(pair);
            println(p,c,pair,ecounts(pair),alignmentLogProbs(pair));
            assert(false);
          }

          val (p: Int,c) = pe.decode(pair);
          // need this for pInsertion
          if(p != pe.epsIndex) {
            nonInsertionCounts += ecounts(pair)
          } else {
            insertionCounts += ecounts(pair)
          }

          weightedSum(deriv,featuresForPair(pair),-ecounts(pair))

          var cPrime = 0;
          while(cPrime < pe.charIndex.size) {
            val pairPrime = pe.encode(p,cPrime);
            if(pairPrime != pe.doubleEpsilon) {
              weightedSum(deriv,featuresForPair(pairPrime),ecounts(pair) * math.exp(alignmentLogProbs(pairPrime)))
            }
            cPrime += 1;
          }
        }
        pair += 1;
      }

      // ll = insertionCounts * log p(insert) + nonInsertionCounts * log p(not Insert)
      // grad = insertionCounts * feats - insertionCounts * (
      logProb += insertionCounts * logProbInsertion
      logProb += nonInsertionCounts * math.log(1-math.exp(logProbInsertion));
      weightedSum(deriv,insertionFeatures(lang),-(insertionCounts - (insertionCounts + nonInsertionCounts) * math.exp(logProbInsertion)));
    }
    assert(!logProb.isNaN);

    println(logProb,norm(deriv,2));
    (-logProb,deriv)
  }
}

object EditDistanceObjectiveFunction {
  trait Feature
  case object Insertion extends Feature;
  case class LangFeature(l: Language, f: Feature) extends Feature;
  case class PairFeature(p : Char, c: Char) extends Feature;
  case class ChildFeature(c: Char) extends Feature;
  case object MatchFeature extends Feature;

  def featuresFor(l: Language, p: Char, c: Char): Array[Feature] = {
    val base = ArrayBuffer[Feature](PairFeature(p,c),ChildFeature(c));
    if(p == c) base += MatchFeature;
    base ++= base.map(LangFeature(l,_));
    base.toArray
  }

  def insertionFeaturesFor(l: Language): Array[Feature] = Array(Insertion,LangFeature(l,Insertion));

  def optimize(index: Index[Char], expectedCounts: Map[(Language,Language),Vector]) = {
    val nicer = expectedCounts.map { case (k,v) => k._2 -> v};
    val pe = new AlignmentPairEncoder(index);
    val obj = new EditDistanceObjectiveFunction(pe,nicer,featuresFor _, insertionFeaturesFor _ );

    val opt = FirstOrderMinimizer.OptParams(useStochastic = false, maxIterations = 50, regularization = 2).minimizer(obj);
    val params = opt.minimize(new CachedDiffFunction(obj), obj.initialWeightVector);
//    println(obj.featureEncoder.decode(params));
    val theMap = expectedCounts.map { case ((parent,child),_) =>
      val matrix = obj.costMatrixFor(child,params);
      def cost(c1: Char, c2: Char) = matrix(index(c1))(index(c2));
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
