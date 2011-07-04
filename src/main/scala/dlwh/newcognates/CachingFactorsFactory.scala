package dlwh.newcognates

import org.apache.commons.collections.SoftRefHashMap;

/**
 * 
 * @author dlwh

class CachingFactorsFactory[F<:SuffStatsFactorsFactory](val inner: F) extends SuffStatsFactorsFactory {
  val cache = collection.JavaConversions.asScalaMap(new SoftRefHashMap());
  def mkFactors(legalWords: Set[Word], edgeParameters: (Language) => EdgeParameters) = {
    null;
  }

  def optimize(suffStats: Map[_root_.dlwh.newcognates.Language, SuffStatsFactorsFactory.

  case class Factors(legalWords: Set[Word], edgeParameters: ) {

  }


  def emptySufficientStatistics = null

  def initialParameters = null

  type EdgeParameters = inner.EdgeParameters;
  case class SufficientStatistics(inner: SufficientStatistics) extends BaseSufficientStatistics {
    def +(stats: SufficientStatistics) = SufficientStatistics(inner + stats.inner);

    def *(weight: Double) = SufficientStatistics(inner * weight);
  }
} */

