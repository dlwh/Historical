package dlwh.historical;

import Math._;

import scalala.Scalala.{log => _, exp => _, sqrt=> _,_};
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
      val langMeanX = new Array[Double](numWaves);
      val langMeanY = new Array[Double](numWaves);
      val featureCounts = Seq.fill(numWaves)(PairedDoubleCounter[Int,Int]());

      // estep:
      for {
        lang <- langs.iterator
        (f,v) <- lang.features.iterator
      } {
        val posterior = DoubleCounter[Int]();
        var logTotal = NEG_INF_DOUBLE;

        for {
          (Wave(wloc,waveFeatures),w) <- state.waves.iterator.zipWithIndex;
          logPLgW = -distSquared(lang.coords,wloc)/waveVariance - log(sqrt(waveVariance * 2 * Pi))
          logPFgW = waveFeatures(f)(v)
        } {
          val joint = logPLgW + logPFgW + state.priors(w);
          posterior(w) = joint;
          assert(!joint.isNaN,(logPLgW,state.priors(w)));
          logTotal = logSum(logTotal,joint);
        }

        ll += logTotal;

        // accumulate suff stats
        for( (w,score) <- posterior ) {
          val prob = Math.exp(score - logTotal);
          assert(!prob.isNaN);
          assert(prob >= 0, (prob, posterior,w));
          featureCounts(w)(f,v) += prob;
          langMeanX(w) +=  prob * lang.coords._1;
          langMeanY(w) +=  prob * lang.coords._2;
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

      langMeanX :/= waveCounts;
      langMeanY :/= waveCounts;
      val newPrior = waveCounts.map(x => log(x) - log(numWaves));
      newPrior foreach { x => assert(!x.isNaN) };
      
      val waves = (for(w <- 0 until numWaves) 
        yield {
        Wave( (langMeanX(w),langMeanY(w)), featureCounts(w));
      }) toSequence;

      State(waves,newPrior,ll,state.newLL);
    }} 

    iterations.steps.takeWhile { x => !converged(x) }
  }

  private def distSquared( x: (Double,Double), y: (Double,Double)) = {
    // horizontal wrap
    val d1 = (x._1 - y._1) min (x._1 + 180 - y._1);
    val d2 = x._2 - y._2;
    d1 * d1 + d2 * d2;
  }

  private def converged(s: State) = {
    (s.oldLL - s.newLL) / s.oldLL  < 1E-5 && s.oldLL != NEG_INF_DOUBLE;
  }

  case class Wave(loc: (Double,Double), features: PairedDoubleCounter[Int,Int]);
  case class State(waves: Seq[Wave], priors: Seq[Double], newLL: Double, oldLL: Double);

  private def initialState(langs: Seq[Language]) = {
    val locMeanX = langs.foldLeft(0.0)( _ + _.coords._1) / langs.length;
    val locMeanY = langs.foldLeft(0.0)( _ + _.coords._2) / langs.length;
    val locs = Array.tabulate(numWaves){i => 
        val (x,y) = langs(langs.length - i - 1).coords;
        ( (x+locMeanX)/2, (y + locMeanY)/2);
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

    println(initState);
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
