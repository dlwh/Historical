package dlwh.historical;

import Math._;

import scalala.Scalala.{log => _, exp => _, _};
import scalanlp.counters._;
import scalanlp.math.Numerics._;
import scalanlp.stats.sampling._; 
import Counters._;
import WALS.Language;

class EM(val numWaves: Int, waveVariance: Double = 1.0) {
  def estimate(langs: Seq[Language]) = {
    val iterations  = MarkovChain(initialState(langs)){ state => Rand.always {
      
      var count = 0;
      var ll = 0.0;
      val langMeanX = new Array[Double](numWaves);
      val langMeanY = new Array[Double](numWaves);
      val featureCounts = Seq.fill(numWaves)(PairedDoubleCounter[Int,Int]());

      // estep:
      for { 
        lang <- langs.iterator;
        (f,v) <- lang.features.iterator
      } {
        val posterior = DoubleCounter[Int]();
        var logTotal = 0.0;

        for {
          (Wave(wloc,waveFeatures),w) <- state.waves.iterator.zipWithIndex;
          logPLgW = -distSquared(lang.coords,wloc)/waveVariance;
          logPFgW = waveFeatures(f)(v)
        } {
          val likelihood = logPLgW + logPFgW; // TODO: prior prob of a wave?
          posterior(w) = likelihood;
          logTotal = logSum(logTotal,likelihood);
          count += 1;
        }

        // logtotal is also datalikelihood:
        ll += logTotal;

        // accumulate suff stats
        for( (w,logProb) <- posterior ) {
          val prob = exp(logProb - logTotal);
          featureCounts(w)(f,v) += prob;
          langMeanX(w) +=  prob * lang.coords._1;
          langMeanY(w) +=  prob * lang.coords._2;
        }

      }

      // m-step:
      for(pairCounter <- featureCounts;
        counts <- pairCounter.counters) {
        val logTotal = log(counts.total);
        // log normalize:
        counts.transform { (f,v) => log(v) - logTotal }
      }

      langMeanX /= count;
      langMeanY /= count;
      val waves = (for(w <- 0 until numWaves) 
        yield {
        Wave( (langMeanX(w),langMeanY(w)), featureCounts(w));
      }) toSequence;

      State(waves,ll,state.newLL);
    }} 

    iterations.samples.takeWhile { x => !converged(x) }
  }

  private def distSquared( x: (Double,Double), y: (Double,Double)) = {
    // horizontal wrap
    val d1 = (x._1 - y._1) min (x._1 + 180 - y._1);
    val d2 = x._2 - y._2;
    d1 * d1 + d2 * d2;
  }

  private def converged(s: State) = {
    (s.newLL - s.oldLL) / s.oldLL  < 1E-4 && s.oldLL != NEG_INF_DOUBLE;
  }

  case class Wave(loc: (Double,Double), features: PairedDoubleCounter[Int,Int]);
  case class State(waves: Seq[Wave], newLL: Double, oldLL: Double);

  private def initialState(langs: Seq[Language]) = {
    val locs = Array.tabulate(numWaves)(i => langs(i).coords)

    // # values for each feature
    val featureSizes = langs.foldLeft(collection.mutable.Map[Int,Int]()) { (map,lang) =>
      for( (f,v) <- lang.features) {
        map(f) = map(f) max v
      }
      map;
    }
    def mapFiller = {
      val ret = PairedDoubleCounter[Int,Int]();
      for( (f,max) <- featureSizes) {
        val logProb = log(1.0 / max);
        for(i <- 1 to max) {
           ret(f,i) = logProb;
        }
      }
      ret;
    }
    val features = Array.fill(numWaves)(mapFiller);
    val waves = for( (l,f) <- locs zip features) yield Wave(l,f);
    State(waves, NEG_INF_DOUBLE, NEG_INF_DOUBLE);
  }
}
