package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

trait SuffStatsFactorsFactory {
  type SufficientStatistics <: BaseSufficientStatistics
  type EdgeParameters;

  trait BaseSufficientStatistics {
    def +(stats: SufficientStatistics):SufficientStatistics
    def *(weight: Double):SufficientStatistics
  }

  def initialParameters: EdgeParameters;
  def emptySufficientStatistics:SufficientStatistics;

  trait HasSufficientStatistics {
    def sufficientStatistics: SufficientStatistics
  }

  trait FactorsWithSuffStats extends dlwh.newcognates.Factors {
    override type Edge <: EdgeBase with HasSufficientStatistics
  }

  type Factors <: FactorsWithSuffStats
  def mkFactors(legalWords: Set[Word], edgeParameters: Language=>EdgeParameters):Factors
  def optimize(suffStats: Map[Language,SufficientStatistics]):Map[Language,EdgeParameters];

}