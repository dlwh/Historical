package dlwh.editdistance

import dlwh.cognates._
import scalanlp.util.Index

/**
 *
 * @author dlwh
 */

trait EditDistance  {
  type Parameters;
  type SufficientStatistics <: BaseSufficientStatistics;

  val charIndex: Index[Char];

  trait BaseSufficientStatistics {
    def +(stats: SufficientStatistics):SufficientStatistics;
    def *(weight: Double):SufficientStatistics;
  }

  def sumCounts[K](s1: Map[K,SufficientStatistics], s2: Map[K,SufficientStatistics]) = {
    val r = collection.mutable.Map[K,SufficientStatistics]();
    r ++= s1;
    for( (k,v) <- s2) {
      if(r.contains(k)) r(k) += v
      else r(k) = v;
    }
    r.toMap
  }

  def initialParameters: Parameters
  def makeParameters(stats: Map[Language,SufficientStatistics]):Map[Language,Parameters]

  def distance(parameters: Parameters, a: String, b: String):Double

  def sufficientStatistics(paremeters: Parameters, a: String, b: String):SufficientStatistics;
  def emptySufficientStatistics:SufficientStatistics
}
