package dlwh.coalescent

import collection.mutable.ArrayBuffer
import breeze.stats.distributions.{Rand, RandBasis, DiscreteDistr, Gamma}

object Coalescents {
  /**
   * A lineage is a sequence of intervals between partitions. lin(0) is the index of the most
   * recent split event. i.e. the one with time closest to 0. Times go back in time,
   * so that all examples were observed to have split at time = 0, and then they merged
   * back in time.
   */
  type Lineage = IndexedSeq[(Double,Seq[Set[Int]])]
}
import Coalescents._

class KingmansCoalescent(n: Int)(implicit rand: RandBasis = Rand) extends DiscreteDistr[Lineage] {
  override def probabilityOf(l: Lineage) = math.exp(logProbabilityOf(l))

  override def logProbabilityOf(pi: Lineage) = {
    require(pi.length == n-1)
    var i = 0
    var lastTime = 0.0
    var score = 0.0
    
    while(i < pi.length) {
      val delta = pi(i)._1 - lastTime
      lastTime += delta
      score -= (n-i) * (n-i-1) / 2 * delta
      i += 1
    }

    score
  }

  def draw(): Lineage = {
    var result = new ArrayBuffer[(Double,Seq[Set[Int]])]

    var partitions : Seq[Set[Int]] = (0 until n) map (Set.empty + _)
    var time = 0.0

    var i = 0
    while(i < n-1) {
      // find a time to merge two guys.
      val rate = (n-i) * (n-i-1) / 2
      val delta = new Gamma(shape = 1.0, scale = 1.0 / rate).draw()
      time += delta
      // pick which two to merge:
      val l = rand.randInt(partitions.length).draw()
      val r = rand.randInt(partitions.length).filter(_ != l).draw

      partitions = partitions.patch(l,Seq(partitions(l) ++ partitions(r)),1).patch(r,Seq.empty,1)
      result += (time -> partitions)
      i += 1
    }

    result:Lineage
  }
}
