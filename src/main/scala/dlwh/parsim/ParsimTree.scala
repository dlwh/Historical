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

/**
 * 
 * @author dlwh
 */

object ParsimTree extends App {
  val configuration = Configuration.fromPropertiesFiles(args.map(new File(_)))
  val dataset = Dataset.fromConfiguration(configuration)
  val langIndex = Index(dataset.languages)
  val cognates = dataset.cognates.flatten;
  val ntaxa = langIndex.size

  val charIndex = Index(cognates.flatMap(_.word))

  // parameters
  val tau = 10000.0 // oldest time
  val gamma0 = 5E-6

  val process = new PPTree(ntaxa, tau, gamma0, IndexedSeq.empty)

  type T = TODO

  def initWeight(x: Int) = -3.
  def transRatio(x: Int, b: Int) = if(x == b) 0 else -4
  val ed = new GeneralEditDistance(1, charIndex, initWeight, initWeight, transRatio)
  val factorsFactory = new InnovationFactorsFactory(new WordFactorsFactory(ed),
                                                    new LanguageModelFactorsFactory(charIndex), 0.2)

  def likelihood[T](initFactors: Factors[T], tree: Tree[Int]):Double = {

    var factors = initFactors
    var inference = new TreeInference(factors, tree, cognates)
    var likelihood = -1E10
    while(true) {
      val newLikelihood : Double = inference.likelihood
      if( (newLikelihood - likelihood)/newLikelihood < 1E-4) return newLikelihood

      likelihood = newLikelihood
      val ecounts = inference.expectedCounts
      factors = optimize(factors,ecounts)
      inference = new TreeInference(factors, tree, cognates)
    }
    likelihood
  }

  val initialFactors = XXX
  var state = process.initialState(likelihood(initialFactors, _ ))
  def ll_data = state.likelihood
  def ll_points = state.prior

  val treeDrawer = new TreeDrawer()

  xlim(0,1000*ntaxa)
  ylim(ll_data * 1.1,0)

  val lls = ArrayBuffer[Double]();

  for(time <- 0 until (1000 * ntaxa)) {
    val temp = (1000.0 * ntaxa - time) / ntaxa / 200
    if(time % 20 == 0) {
      println("[temp %.3f] [ll %.2f %.2f %.2f] [root %.2f]"
        .format(temp, ll_points + ll_data, ll_points, ll_data, state.appliedEvents.last.time))
      plot(DenseVector.range(0,time),lls.toArray)
    }
    if(time % 200 == 199) {
      val ss = state
      import ss._
      println(points.map(_.map(langIndex.get _)))
      println(appliedEvents.map(_.map(langIndex.get _)))
      treeDrawer.trees = forest.map(_.map(i => if(i < 0) "" else langIndex.get(i)))
      for( tree <- forest) println(tree.map(i => if(i < 0) "" else langIndex.get(i)))
      println("Saving points...")
      new java.io.File("serialized").mkdirs()
      scalanlp.util.writeObject(new java.io.File("serialized/k2p-events-" + (time+1) +".ser"), appliedEvents.map(_.map(langIndex.get _)))
    }

    state = process.resample(state, likelihood(initialFactors, _), temp)

    lls += (ll_points + ll_data)
  }


}