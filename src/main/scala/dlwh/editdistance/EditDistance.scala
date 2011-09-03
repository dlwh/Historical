package dlwh.editdistance

import dlwh.cognates._
import scalanlp.stats.distributions.{SufficientStatistic=>BaseSufficientStatistic}
import scalanlp.util.Index

/**
 *
 * @author dlwh
 */

trait EditDistance {
  type Parameters;
  type SufficientStatistic <: BaseSufficientStatistic[SufficientStatistic];

  val charIndex: Index[Char];

  def sumCounts[K](s1: Map[K,SufficientStatistic], s2: Map[K,SufficientStatistic]) = {
    val r = collection.mutable.Map[K,SufficientStatistic]();
    r ++= s1;
    for( (k,v) <- s2) {
      if(r.contains(k)) r(k) += v
      else r(k) = v;
    }
    r.toMap
  }

  def initialParameters: Parameters
  def makeParameters[K](stats: Map[K,SufficientStatistic]):Map[K,Parameters]

  def distance(parameters: Parameters, a: String, b: String):Double

  def sufficientStatistics(paremeters: Parameters, a: String, b: String):SufficientStatistic;
  def emptySufficientStatistic:SufficientStatistic
}
