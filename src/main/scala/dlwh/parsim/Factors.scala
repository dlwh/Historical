package dlwh.parsim

import dlwh.cognates._
import scalanlp.stats.distributions.{SufficientStatistic => BaseSuffStats}

/**
 * 
 * @author dlwh
 */

trait FactorsFactory { ff =>
  type Factors[T] <: dlwh.parsim.Factors[T] {
    type SufficientStatistic <: ff.SufficientStatistic
  }
  type EdgeParameters;
  type SufficientStatistic <: BaseSuffStats[SufficientStatistic]

  def emptySufficientStatistic: SufficientStatistic
  def initialParameters: EdgeParameters

  def optimize[T](stats: Map[T,SufficientStatistic]):Map[T,EdgeParameters]
  def factorsFor[T](legalWords: Set[Word], t: Map[T,EdgeParameters]):Factors[T]
}

trait Factors[T] { outer =>
  type Belief <: BaseBelief
  type SufficientStatistic <: BaseSuffStats[SufficientStatistic]
  type Edge <: BaseEdge;
  type EdgeMarginal <: BaseEdgeMarginal

  def rootMessage(language: T): Belief
  def indicatorBelief(w: Word):Belief
  def uniformBelief: Belief
  def edgeFor(t: T):Edge

  trait BaseBelief { this: Belief =>
    def apply(w: Word):Double
    def partition: Double
    def *(b: Belief):Belief
    def normalized:Belief = scaleBy(-partition)
    def scaleBy(scale: Double):Belief
  }

  trait BaseEdge {
    def edgeMarginal(parent: Belief, child: Belief):EdgeMarginal
    def score(parent: Word, child: Word):Double
  }

  trait BaseEdgeMarginal {
    def sufficientStatistic:SufficientStatistic
    def parentProjection:Belief
    def childProjection:Belief
  }

}
