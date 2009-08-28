package dlwh.historical;

import Math._;

import java.io._;

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

    val iterations  = MarkovChain(initialState(langs)){ state => Rand.always {
      var waveCounts = new Array[Double](numWaves);
      var ll = 0.0;
      val waveMean = Array.fill(numWaves)(Vector2());
      val featureCounts = Seq.fill(numWaves)(PairedDoubleCounter[Int,Int]());
      val waveCovariances = Array.fill(numWaves)(zeros(2,2));

      // estep:
      for {
        lang <- langs.iterator
        (f,v) <- lang.features.iterator
      } {
        val (posterior,logTotal) = posteriorForFeature(state,lang,f,v);

        ll += logTotal;

        // accumulate suff stats
        for( (w,score) <- posterior ) {
          val prob = Math.exp(score - logTotal);
          assert(prob >= 0, (prob, posterior,w));
          featureCounts(w)(f,v) += prob;
          waveMean(w) += lang.coords * prob;
          // Should actually do wave Covariances posthoc. Oh well.
          val dist = wrapDist(lang.coords,state.waves(w).loc);
          waveCovariances(w) += dist * dist.t * prob;
          waveCounts(w) += prob;
        }

      }

      // m-step:
      for(pairCounter <- featureCounts;
          counts <- pairCounter.counters) {
        val logTotal = log(counts.total + 0.0001 * counts.size);
        // log normalize:
        counts.transform { (f,v) =>
        assert(v >= 0);
        val r = log(v + 0.0001) - logTotal
        assert(!r.isNaN,(logTotal,counts));
        r;
      }
      }
      println(waveCounts.mkString(","));

      val actualWaveMean = Array.tabulate(numWaves){ i => waveMean(i)/waveCounts(i) value };
      val actualWaveCov = Array.tabulate(numWaves){ i =>
         (waveCovariances(i) + eye(2) * 300) / (10.0 + waveCounts(i)) value
      }
      val actualWaveICov = actualWaveCov map { cov =>
         val I : DenseMatrix = eye(2)
         val icov = cov \ I value;
         icov
      }

      val newPrior = waveCounts.map(x => log(x + 10.0) - log(10.0 * numWaves + waveCounts.foldLeft(0.0)(_+_)));
      newPrior foreach { x => assert(!x.isNaN) };

      val waves = (for(w <- 0 until numWaves)
        yield {
        Wave( actualWaveMean(w), actualWaveCov(w), actualWaveICov(w), featureCounts(w));
      }) toSequence;

      State(waves,newPrior,ll,state.newLL);
    }}

    iterations.steps.takeWhile { x => !converged(x) }
  }

  def posteriorForFeature(state: State, lang: Language, f: Int, v: Int) = {
    val posterior = DoubleCounter[Int]();
    var logTotal = NEG_INF_DOUBLE;

    for {
      (Wave(wloc,_,wicov,waveFeatures),w) <- state.waves.iterator.zipWithIndex;
      logPLgW = logGaussianProb(lang.coords,wloc,wicov)
      logPFgW = waveFeatures(f)(v)
    } {
      val joint = logPLgW + logPFgW + state.priors(w);
      posterior(w) = joint;
      assert(!joint.isNaN,(logPLgW,state.priors(w)));
      logTotal = logSum(logTotal,joint);
    }

    (posterior,logTotal);
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
  case class Wave(loc: Vector, cov: Matrix, icov: Matrix, features: PairedDoubleCounter[Int,Int]);
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
    val features = Array.fill(numWaves)(mapFiller);
    val waves = for( (((l,f),ic),c) <- locs zip features zip icovs zip covs) yield Wave(l,c,ic,f);
    val initState = State(waves, Array.fill(numWaves)(log(1.0/numWaves)), NEG_INF_DOUBLE, NEG_INF_DOUBLE);

    initState;
  }
}

trait MainEM {
  def data: Seq[Language];

  def main(args: Array[String]) {
    val em = new EM(50);
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

    val output = new PrintWriter(new BufferedWriter(new FileWriter(new java.io.File("origins.txt"))));
    for(l <- data.elements;
        () = { output.println("====");output.println(l.name); output.println(l.coords); };
        (f,v) <- l.features.elements) {
      val (posterior,_) = em.posteriorForFeature(last,l,f,v);
      val w = posterior.argmax;
      output.println("Feature " + f + " is " + v + " from: ");
      output.println(w +" " + last.waves(w).loc);
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
