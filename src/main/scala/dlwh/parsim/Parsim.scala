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
  val gold = new CognatesEval(dataset.cognates.map(_.filter(c => leaves(c.language))));

  val ntaxa = langIndex.size

  val charIndex = Index(cognates.flatMap(_.word) ++  Iterator('\0'))

  // parameters
  val tau = 10000.0 // oldest time
  val gamma0 = 5E-6

  type T = Set[Language]

  def initWeight(x: Int) = -4. + x
  def subWeight(x: Int) = -4. - x
  def transRatio(x: Int, b: Int) = if(x == b) 0.0 else -1.
  val ed = new GeneralEditDistance(1, charIndex, subWeight, initWeight, transRatio)
  val factorsFactory = new InnovationFactorsFactory(new WordFactorsFactory(ed),
                                                    new LanguageModelFactorsFactory(charIndex), 0.4)

  import factorsFactory.Factors


  var decoded:IndexedSeq[IndexedSeq[Cognate]] = null
  def likelihood(initParams: Map[T,factorsFactory.EdgeParameters], annTree: Tree[T]):Double = {
    def cognateFor(group: Map[Language,IndexedSeq[Cognate]], lang: Set[Language]) = {
      if(lang.size == 1) group.get(lang.iterator.next).getOrElse(Seq.empty)
      else Seq.empty
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
      val change = (newLikelihood - likelihood) / newLikelihood.abs
      decoded = inference.par.flatMap(decodeCognates(_,annTree)).seq.toIndexedSeq

      val (p,r)  = gold.precisionAndRecall(decoded.map(g => CognateGroup(g:_*)))
      val f1 = 2 * p * r / (p + r)

      println(newLikelihood,likelihood,change,p,r,f1)
      if( change < 1E-4 && iter > 3) return newLikelihood

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

  def decodeCognates(inference: TreeInference[T,factorsFactory.Factors[T]],
                     tree: Tree[T],
                     cur: ArrayBuffer[Cognate]= new ArrayBuffer[Cognate]):IndexedSeq[IndexedSeq[Cognate]] = {
    val label = tree.label
    val children = tree.children
    val childGroups = for(c <- children if inference.hasUpwardMessage(c.label)) yield {
      val edges = inference.edgeMarginal(c.label);
      val fromChild = if(c.isLeaf) {
        var rest = IndexedSeq.empty[Cognate];
        for( (w,e) <- inference.cognates(c.label) zip edges) {
          if(e.posteriorInnovationProb >= 0.5) rest :+= w
          else cur += w
        }
        rest.map(IndexedSeq(_));
      } else if(edges.head.posteriorInnovationProb < 0.5){
        decodeCognates(inference,c,cur).drop(1)
      } else {
        decodeCognates(inference,c);
      }
      fromChild;
    }

    (cur +: (childGroups.flatten.filterNot(_.isEmpty))).toIndexedSeq;
  }
}