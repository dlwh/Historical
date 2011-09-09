package dlwh.parsim

import phylo.{Dataset => _, _}
import scalanlp.config.Configuration
import java.io.File
import dlwh.cognates._
import scalanlp.util.Index
import math._
import scalanlp.util._
import scalala.library.Plotting._
import scalala.tensor.dense.DenseVector
import scalanlp.stats.distributions.{Rand, Poisson}
import collection.mutable.ArrayBuffer
import dlwh.editdistance.GeneralEditDistance
import collection.immutable.Map
import collection.IndexedSeq

/**
 * 
 * @author dlwh
 */
object Parsim extends App {

  val configuration = Configuration.fromPropertiesFiles(args.map(new File(_)))
  val dataset = Dataset.fromConfiguration(configuration)
  val langIndex = Index(dataset.languages)
  val baseTree = dataset.tree
  val tree = dataset.tree.extend(_.leaves.toSet)
  val leaves = baseTree.leaves.toSet
  val cognates = dataset.cognates.flatten.filter(c => leaves(c.language));
  val groupedCognates = groupCognates(cognates)

  val ntaxa = langIndex.size

  val charIndex = Index(cognates.flatMap(_.word) ++  Iterator('\0'))

  // parameters
  val tau = 10000.0 // oldest time
  val gamma0 = 5E-6

  type T = Set[Language]

  def initWeight(x: Int) = -3.
  def transRatio(x: Int, b: Int) = if(x == b) 0 else -4
  val ed = new GeneralEditDistance(1, charIndex, initWeight, initWeight, transRatio)
  val factorsFactory = new InnovationFactorsFactory(new WordFactorsFactory(ed),
                                                    new LanguageModelFactorsFactory(charIndex), 0.2)

  import factorsFactory.Factors


  def likelihood(initParams: Map[T,factorsFactory.EdgeParameters], annTree: Tree[T]):Double = {
    def cognateFor(group: Map[Language,IndexedSeq[Cognate]], lang: Set[Language]) = {
      if(lang.size == 1) group.get(lang.iterator.next).map(_.head)
      else None
    }

    var params = initParams

    var likelihood = -1E10
    var iter = 0;
    while(true) {
      val inference = for ( c <- groupedCognates.par) yield {
        val factors = factorsFactory.factorsFor(c.values.flatMap(_.map(_.word)).toSet,params)
        new TreeInference(factors, annTree, { (x:T) => cognateFor(c, x)})
      }
      val newLikelihood : Double = inference.par.map(_.likelihood).sum
      println(newLikelihood)
      if( (newLikelihood - likelihood)/newLikelihood.abs < 1E-4 && iter > 3) return newLikelihood

      likelihood = newLikelihood
      val ecounts = inference.par.map(_.sufficientStatistics).reduceLeft(sumStats _)
      assert(ecounts.size > 0)
      params = factorsFactory.optimize(ecounts)
      iter += 1
    }
    likelihood
  }

  val initParams = {for(t <- tree.postorder) yield (t.label) -> factorsFactory.initialParameters}.toMap

  likelihood(initParams, tree)


  // helper methods:
  def groupCognates(cognates: IndexedSeq[Cognate]): IndexedSeq[Map[Language, IndexedSeq[Cognate]]] = {
    cognates.groupBy(_.gloss).mapValues(_.groupBy(_.language)).values.toIndexedSeq
  }

  def sumStats[T](a: Map[T,factorsFactory.SufficientStatistic], b: Map[T,factorsFactory.SufficientStatistic]) = {
    val result = collection.mutable.Map[T,factorsFactory.SufficientStatistic]()
    result ++= a
    for( (k,t) <- b) {
      if(result.contains(k)) {
        result(k) += t
      } else {
        result(k) = t
      }
    }
    result.toMap

  }
}