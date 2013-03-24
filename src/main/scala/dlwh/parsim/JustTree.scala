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
import phylo.inference._

/**
 * 
 * @author dlwh
 */

object JustTree extends App {
  val configuration = Configuration.fromPropertiesFiles(args.map(new File(_)))
  val dataset = Dataset.fromConfiguration(configuration)
  val goldTree = dataset.tree
  println(goldTree)
  val leaves = goldTree.leaves.toSet
  val cognates = dataset.cognates
  val langIndex = Index(dataset.languages.filter(leaves))
  val ntaxa = langIndex.size
  val binarized:IndexedSeq[Array[Int]] = binarizeCognates(langIndex, cognates)
  val gold = new CognatesEval(dataset.cognates.map(_.filter(c => leaves(c.language))));


  // parameters
  val tau = 10000.0 // oldest time
  val gamma0 = 5E-6

  val process = new PPTree(ntaxa, tau, gamma0, IndexedSeq.tabulate(ntaxa)(n => new AttestationEvent(n, 0.0)) ++ IndexedSeq(OriginEvent(10000)))

  var rate = 1E-5
  def likelihood(tree: Tree[Int]):Double = {
    val inf = new K2PInference(rate)
    val likelihood = binarized.par.map(inf.likelihood(tree,_)).sum
    likelihood
  }

  var state = process.initialState(likelihood(_ ))
  def ll_data = state.likelihood
  def ll_points = state.prior

//  val treeDrawer = new TreeDrawer()

//  xlim(0,1000*ntaxa)
//  ylim(ll_data * 1.1,0)

  val lls = ArrayBuffer[Double]();

  for(time <- 0 until (1000 * ntaxa)) {
    val temp = (1000.0 * ntaxa - time) / ntaxa / 200
    if(time % 10 == 0) {
      println(state.points, state.appliedEvents)
      println(time + " [temp %.3f] [ll %.2f %.2f %.2f] [root %.2f]"
        .format(temp, ll_points + ll_data, ll_points, ll_data, state.appliedEvents.last.time))
//      plot(DenseVector.range(0,time),lls.toArray)
    }
    if(time % 200 == 199) {
      val ss = state
      import ss._
      println(points.map(_.map(langIndex.get _)))
      println(appliedEvents.map(_.map(langIndex.get _)))
//      treeDrawer.trees = forest.map(_.map(i => if(i < 0) "" else langIndex.get(i)))
      for( tree <- forest) println(tree.map(i => if(i < 0) "" else langIndex.get(i)))
      println("Saving points...")
      new java.io.File("serialized").mkdirs()
      scalanlp.util.writeObject(new java.io.File("serialized/k2p-events-" + (time+1) +".ser"), appliedEvents.map(_.map(langIndex.get _)))
    }

    val oldstate = state
    state = process.resample(state, likelihood(_), temp)
    if(state.forest != oldstate.forest) {
      println("New forest");
      state.forest foreach { f => println(f.map(i => if(i < 0) "" else langIndex.get(i)))}
    }

    lls += (ll_points + ll_data)
  }


  // helper methods:
  def binarizeCognates(langIndex: Index[Language], cognates: IndexedSeq[Seq[Cognate]]) = {
    cognates.map(vec => Encoder.fromIndex(langIndex).tabulateArray(l => if(vec.find(_.language ==l).nonEmpty) 1 else 0))
  }

}


