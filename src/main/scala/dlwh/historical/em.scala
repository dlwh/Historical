package dlwh.historical;

import Math._;

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
        val posterior = DoubleCounter[Int]();
        var logTotal = NEG_INF_DOUBLE;

        for {
          (Wave(wloc,waveFeatures),w) <- state.waves.iterator.zipWithIndex;
          logPLgW = logGaussianProb(lang.coords,wloc)
          logPFgW = waveFeatures(f)(v)
        } {
          val joint = logPLgW + logPFgW + state.priors(w);
          posterior(w) = joint;
          assert(!joint.isNaN,(logPLgW,state.priors(w)));
          logTotal = logSum(logTotal,joint);
        }

        ll += logTotal;

        // accumulae suff stats
        for( (w,score) <- posterior ) {
          val prob = Math.exp(score - logTotal);
          assert(!prob.isNaN);
          assert(prob >= 0, (prob, posterior,w));
          featureCounts(w)(f,v) += prob;
          assert(lang != null)
          waveMean(w) += lang.coords * prob;
          // Should actually do wave Covariances posthoc. Oh well.
          val dist = wrapDist(lang.coords,state.waves(w).loc);
          waveCovariances(w) += dist.t * dist * prob;
          waveCounts(w) += prob;
        }

      }

      // m-step:
      for(pairCounter <- featureCounts;
        counts <- pairCounter.counters) {
        val logTotal = log(counts.total);
        // log normalize:
        counts.transform { (f,v) =>
          assert(v >= 0);
          val r = log(v) - logTotal
          assert(!r.isNaN);
          r;
        }
      }

      val actualWaveMean = Array.tabulate(numWaves){ i => waveMean(i)/waveCounts(i) value };
      val actualWaveCov = Array.tabulate(numWaves){ i => waveCovariances(i)/waveCounts(i) value };

      val newPrior = waveCounts.map(x => log(x) - log(numWaves));
      newPrior foreach { x => assert(!x.isNaN) };
      
      val waves = (for(w <- 0 until numWaves) 
        yield {
        Wave( actualWaveMean(w), featureCounts(w));
      }) toSequence;

      State(waves,newPrior,ll,state.newLL);
    }} 

    iterations.steps.takeWhile { x => !converged(x) }
  }

  // horizontal wrap
  private def wrapDist(x: Vector, y: Vector) = {
    val diff = (x - y) value;
    diff(1) = diff(1) min (x(1) + 180.0 - y(1));
    diff
  }

  private def logGaussianProb( x: Vector, y: Vector) = {
    val diff = wrapDist(x,y);
    -(diff dot diff)/waveVariance - log(sqrt(waveVariance * 2 * Pi));
  }

  private def converged(s: State) = {
    (s.oldLL - s.newLL) / s.oldLL  < 1E-5 && s.oldLL != NEG_INF_DOUBLE;
  }

  case class Wave(loc: Vector, features: PairedDoubleCounter[Int,Int]);
  case class State(waves: Seq[Wave], priors: Seq[Double], newLL: Double, oldLL: Double);

  private def initialState(langs: Seq[Language]) = {
    val locMean = mean(langs.map(_.coords));
    val locs = Array.tabulate(numWaves){i => 
        val coord = langs(langs.length - i - 1).coords;
        (coord + locMean) / 2 value
    }

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
    val waves = for( (l,f) <- locs zip features) yield Wave(l,f);
    val initState = State(waves, Array.fill(numWaves)(log(1.0/numWaves)), NEG_INF_DOUBLE, NEG_INF_DOUBLE);

    initState;
  }
}

object RunAll {
  def main(args: Array[String]) {
    val em = new EM(50);
    val data = WALS.daumeAll;
    em.estimate(data).foreach { i =>
      i.waves.map(_.loc) foreach println


        hold(false);
        Plot.wals(data);
        hold(true);
        for( w <- i.waves;
          loc = w.loc) {
          Plot.circle(loc,sqrt(em.waveVariance));
        }

      println("old LL: " + i.oldLL);
      println("LL: " + i.newLL);
      println("Rel change: " + (i.newLL - i.oldLL)/i.oldLL);
    }
  }
}

object RunIE {
  def main(args: Array[String]) {
    val em = new EM(25);
    val data = WALS.daumeIE;
    em.estimate(data).zipWithIndex.foreach { case(i,num) =>
      i.waves.map(_.loc) foreach println

      if(num % 2 == 0) { 
        hold(false);
        Plot.wals(data);
        hold(true);
        for( w <- i.waves;
          loc = w.loc) {
          Plot.circle(loc,sqrt(em.waveVariance));
        }
      }

      println("old LL: " + i.oldLL);
      println("LL: " + i.newLL);
    }
  }
}