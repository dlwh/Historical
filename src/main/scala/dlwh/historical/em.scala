package dlwh.historical;

import Math._;

import java.io._;

import scala.collection.mutable.ArrayBuffer;
import scala.collection.immutable.HashMap;

import scalala.Scalala.{log => _, exp => _, sqrt=> _,_};
import scalala.tensor._;
import scalala.tensor.fixed._;
import scalala.tensor.dense._;
import scalanlp.counters._;
import scalanlp.math.Numerics._;
import scalanlp.stats.sampling._;
import Counters._;
import WALS.Language;

class EM(val numWaves: Int, val waveVariance: Double = 50.0) {
  def estimate(langs: Seq[Language]) = {

    val validValuesForFeature = langs.iterator.flatMap(_.features.iterator).foldLeft(Map.empty[Int,Set[Int]]) { (map,fv) =>
      val newSet = map.get(fv._1).map{ _ + fv._2 }.getOrElse(Set(fv._2))
      map + (fv._1 -> newSet)
    };

    val iterations  = MarkovChain(initialState(langs)){ state => Rand.always {
      var waveCounts = new Array[Double](numWaves);
      var ll = 0.0;
      val waveMean = Array.fill(numWaves)(Vector2());
      val typeCounts = Seq.fill(numWaves)(DoubleCounter[Int]());
      val featureCounts = Seq.fill(numWaves)(PairedDoubleCounter[Int,Int]());
      val waveCovariances = Array.fill(numWaves)(zeros(2,2));

      // estep:
      for {
        lang <- langs.iterator
        (f,vs) <- validValuesForFeature.iterator
      } {
        val featureSet = lang.features.get(f).map(x => Set(x)) getOrElse vs;
        
        val posterior = posteriorForFeature(state,lang,f,featureSet);
        // LogPairedDoubleCounter[Wave,Value]

        ll += posterior.logTotal;

        // accumulate suff stats
        for( (w,ctr) <- posterior.rows ) {
          for( (v,score) <- ctr) {
            val probVgZ = Math.exp(score - ctr.logTotal);
            assert(probVgZ >= 0, (probVgZ, posterior,w));
            featureCounts(w)(f,v) += probVgZ;
          }
          val probZ = Math.exp(ctr.logTotal - posterior.logTotal);
          typeCounts(w)(f) += probZ;
          waveMean(w) += lang.coords * probZ;
          // Should actually do wave Covariances posthoc. Oh well.
          val dist = wrapDist(lang.coords,state.waves(w).loc);
          waveCovariances(w) += dist * dist.t * probZ;
          waveCounts(w) += probZ;
        }
      }

      // m-step:
      for(pairCounter <- featureCounts;
          (_,counts) <- pairCounter.rows) {
        val logTotal = log(counts.total + 0.0001 * counts.size);
        // log normalize:
        counts.transform { (f,v) =>
          assert(v >= 0);
          val r = log(v + 0.0001) - logTotal
          assert(!r.isNaN,(logTotal,counts));
          r;
        }
      }

      for(counts <- typeCounts) {
        val logTotal = log(counts.total + 0.0001 * counts.size);
        counts.transform { (f,v) =>
          assert(v >= 0);
          val r = log(v + 0.0001) - logTotal
          assert(!r.isNaN,(logTotal,counts));
          r;
        }
      }

      val actualWaveMean = Array.tabulate(numWaves){ i => waveMean(i)/waveCounts(i) value };

      val actualWaveCov = Array.tabulate(numWaves){ i =>
         (waveCovariances(i) + eye(2) * 1000) / (10.0 + waveCounts(i)) value
      }
      val actualWaveICov = actualWaveCov map { cov =>
         val I : DenseMatrix = eye(2)
         val icov = cov \ I value;
         icov
      }

      //val newPrior = waveCounts.map(x => log(x + 100.0) - log(100.0 * numWaves + waveCounts.foldLeft(0.0)(_+_)));
      val newPrior = waveCounts.map(x => log(1.0 / numWaves));
      newPrior foreach { x => assert(!x.isNaN) };

      val waves = (for(w <- 0 until numWaves)
        yield {
        Wave( actualWaveMean(w), actualWaveCov(w), actualWaveICov(w), typeCounts(w), featureCounts(w));
      }) toSequence;

      State(waves,newPrior,ll,state.newLL);
    }}

    iterations.steps.takeWhile { x => !converged(x) }
  }

  def posteriorForFeature(state: State, lang: Language, f: Int, v: Int) = {
    val posterior = LogCounters.LogDoubleCounter[Int]();

    for {
      (Wave(wloc,_,wicov,waveTypes,waveFeatures),w) <- state.waves.iterator.zipWithIndex;
      logPLgW = logGaussianProb(lang.coords,wloc,wicov) / 1.2
      logPVgWF = waveFeatures(f)(v) * 1.1
      logPFgW = waveTypes(f) * 1.1
    } {
      val joint = logPLgW + logPFgW + logPVgWF + state.priors(w);
      posterior(w) = joint;
      assert(!joint.isNaN,(logPLgW,state.priors(w)));
    }

    posterior;
  }


  def posteriorForFeature(state: State, lang: Language, f: Int, values: Set[Int]) = {
    val posterior = LogCounters.LogPairedDoubleCounter[Int,Int]();

    for {
      (Wave(wloc,_,wicov,waveTypes,waveFeatures),w) <- state.waves.iterator.zipWithIndex;
      logPLgW = logGaussianProb(lang.coords,wloc,wicov) / 1.2
      v <- values;
      logPVgWF = waveFeatures(f)(v) * 1.1
      logPFgW = waveTypes(f) * 1.1
    } {
      val joint = logPLgW + logPFgW + logPVgWF + state.priors(w);
      posterior(w,v) = joint;
    }

    posterior
  }




  // horizontal wrap
  private def wrapDist(x: Vector, y: Vector) = {
    val diff = (x - y) value;
    diff(1) = diff(1) min (x(1) + 180.0 - y(1));
    diff
  }

  private def logGaussianProb(x: Vector, y: Vector, icov: Matrix) = {
    val diff = wrapDist(x,y);
    val determinant = icov(0,0) * icov(1,1) - icov(1,0) * icov(0,1);
    -.5 * (diff.t * icov * diff) - log(sqrt(1/determinant) * 2 * Pi) ;
  }

  private def converged(s: State) = {
    (s.oldLL - s.newLL) / s.oldLL  < 1E-4 && s.oldLL != NEG_INF_DOUBLE;
  }

  // icov is inverse covariance
  case class Wave(loc: Vector, cov: Matrix, icov: Matrix, featureTypes: DoubleCounter[Int], features: PairedDoubleCounter[Int,Int]);
  case class State(waves: Seq[Wave], priors: Seq[Double], newLL: Double, oldLL: Double);

  private def initialState(langs: Seq[Language]) = {
    val locMean = mean(langs.map(_.coords));
    val locs = Array.tabulate(numWaves){i =>
      val coord = langs(langs.length - i - 1).coords;
      (coord + locMean) / 2 value
    }

    val icovs = Array.fill(numWaves)(eye(2) / waveVariance value);
    val  covs = Array.fill(numWaves)(eye(2) * waveVariance value);

    // # values for each feature
    val featureSizes = langs.foldLeft(collection.mutable.Map[Int,Int]()) { (map,lang) =>
      for( (f,v) <- lang.features) {
        map.get(f) match {
          case Some(i) => map(f) = v max i
          case None => map(f) = v
        }
      }
      map;
    }

    def mapFiller = {
      val ret = PairedDoubleCounter[Int,Int]();
      for( (f,max) <- featureSizes) {
        val logProb = log(1.0 / max);
        for(i <- 0 until max) {
           ret(f,i) = logProb;
        }
      }
      ret;
    }

    def typeFiller = {
      val ret = DoubleCounter[Int]();
      for(k <- featureSizes.keysIterator) {
        ret(k) = Math.log(1.0/featureSizes.size);
      }
      ret
    }

    val features = Array.fill(numWaves)(mapFiller);
    val featureTypes = Array.fill(numWaves)(typeFiller);
    val waves = Array.tabulate(numWaves) { i => 
      Wave(locs(i),covs(i),icovs(i),featureTypes(i),features(i));
    }
    val initState = State(waves, Array.fill(numWaves)(log(1.0/numWaves)), NEG_INF_DOUBLE, NEG_INF_DOUBLE);

    initState;
  }
}

trait MainEM {
  def data: Seq[Language];
  val numWaves = 15;

  def main(args: Array[String]) {
    val em = new EM(numWaves);
    var last : em.State = null;
    em.estimate(data).zipWithIndex.foreach { case(i,num) =>
      i.waves.map(_.loc) foreach println

      if(num % 2 == 0) {
        hold(false);
        Plot.wals(data);
        hold(true);
        for( w <- i.waves;
          loc = w.loc) {
          Plot.gaussian(loc,w.cov);
        }
      }

      println("old LL: " + i.oldLL);
      println("LL: " + i.newLL);
      println("Rel change: " + (i.oldLL - i.newLL)/i.oldLL);
      last = i;
    }

    val families = Array.fill(numWaves)(DoubleCounter[String]());
    val geni = Array.fill(numWaves)(DoubleCounter[(String,String)]());
    val names = Array.fill(numWaves)(DoubleCounter[String]());
    val tpes = Array.fill(numWaves)(PairedDoubleCounter[Int,Int]());

    val output = new PrintWriter(new BufferedWriter(new FileWriter(new java.io.File("origins.txt"))));
    for(l <- data.iterator) {
      output.println("====");
      output.println(l.name);
      output.println(l.coords); 
      val ctr = DoubleCounter[Int];
      for((f,v) <- l.features.iterator) {
        val posterior = LogCounters.normalize(em.posteriorForFeature(last,l,f,v));
        ctr += posterior;
        for( (w,score) <- posterior) {
          families(w)(l.family) += score;
          geni(w)((l.family,l.genus)) += score;
          names(w)(l.name) += score;
          tpes(w)(f,v) += score;
        }
      };
      output.println( (ctr / ctr.total value).filter{ case(k,v) => v > 0.05})
    }

    for(w <- 0 until em.numWaves) {
      output.println("==========");
      output.println("Home Wave " + w + " " + last.waves(w).loc);
      output.println(families(w));
      output.println(geni(w));
      output.println(names(w));
      output.println(tpes(w));
    }
    output.close();

  }

}

object RunAll extends MainEM {
  val data = WALS.daumeAll;
}

object RunIE extends MainEM {
  val data = WALS.daumeIE;
}
