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
  val attested_by_num = Array.fill(ntaxa)(0.0)
  val attestedEvents = for ( (t,a) <- attested_by_num.zipWithIndex) yield AttestationEvent(a,t)

  // parameters
  val tau = 10000.0 // oldest time
  val gamma0 = 5E-6

  // initialize points with PPP on Omega
  var points = (
    BorrowingEvent.randBorrowingEvent(ntaxa, tau)
      .sample(new Poisson(ntaxa * ntaxa * tau * gamma0).draw() max 10)
      .toIndexedSeq
      .sortBy((x:BorrowingEvent[Int])=>x.time)
    )
  def n_points = points.size
  println(n_points, tau * gamma0)

  val gamma = gamma0 * tau * ntaxa * (ntaxa - 1)
  def ll_points = n_points * log( gamma) * -gamma - (0 until points.size).map(i => log(i + 1)).sum

  var (forest, appliedEvents) = BorrowingEvent.treeFromBorrowingEvents(points, ntaxa)

  def likelihood(initFactors: Factors, tree: Tree[Language]):Double = {
    var factors = initFactors
    var inference = new TreeInference(factors, tree, cognates)
    var likelihood = -1E10
    while(true) {
      val newLikelihood : Double = inference.likelihood
      if( (newLikelihood - likelihood)/newLikelihood < 1E-4) return newLikelihood

      likelihood = newLikelihood
      val ecounts = inference.expectedCounts
      factors = optimize(factors,ecounts)
      inference = new TreeInference(tree, factors, cognates)
    }
    likelihood
  }

  // log likelihood of a forest, reusing old forest is possible
  def compute_ll(newTrees: scala.IndexedSeq[Tree[Int]]): Double = {
    val ll = newTrees.map(_.map(langIndex.get _)).map(likelihood(TODO, _)).sum
    ll
  }

  def compute_ll_data_ratio(new_appliedevents: IndexedSeq[BorrowingEvent[Int]],
                            newTrees: IndexedSeq[Tree[Int]], oldLL: Double, reuse:Boolean = true) = {
    if(new_appliedevents == appliedEvents && reuse) { 0.0}
    else {
      val ll: Double = compute_ll(newTrees)
      assert(!ll.isNaN && !ll.isInfinite)
      ll - oldLL
    }
  }

  val treeDrawer = new TreeDrawer(forest.map(_.map(i => if(i < 0) "" else langIndex.get(i))))

  val initialFactors = XXX
  var ll_data = compute_ll(forest)

  xlim(0,1000*ntaxa)
  ylim(ll_data * 1.1,0)

  val lls = ArrayBuffer[Double]();

  for(time <- 0 until (1000 * ntaxa)) {
    val temp = (1000.0 * ntaxa - time) / ntaxa / 200
    if(time % 20 == 0) {
      println("[temp %.3f] [ll %.2f %.2f %.2f] [root %.2f]"
        .format(temp, ll_points + ll_data, ll_points, ll_data, appliedEvents.last.time))
      plot(DenseVector.range(0,time),lls.toArray)
    }
    if(time % 200 == 199) {
      println(points.map(_.map(langIndex.get _)))
      println(appliedEvents.map(_.map(langIndex.get _)))
      treeDrawer.trees = forest.map(_.map(i => if(i < 0) "" else langIndex.get(i)))
      for( tree <- forest) println(tree.map(i => if(i < 0) "" else langIndex.get(i)))
      println("Saving points...")
      new java.io.File("serialized").mkdirs()
      scalanlp.util.writeObject(new java.io.File("serialized/k2p-events-" + (time+1) +".ser"), appliedEvents.map(_.map(langIndex.get _)))
    }


    // move to add contact edge
    {
      val new_point =  BorrowingEvent.randBorrowingEvent(ntaxa, tau).get()
      val new_points = points :+ new_point
      val ll_points_ratio = log( gamma / (n_points + 1))

      val (new_trees, new_appliedevents) = BorrowingEvent.treeFromBorrowingEvents(attestedEvents ++ new_points, ntaxa)
      val ll_data_ratio = compute_ll_data_ratio(new_appliedevents, new_trees, ll_data)
      val ll_ratio = ll_points_ratio + ll_data_ratio
      if (log(random) <= (ll_ratio)/temp) { // accept
        points = new_points
        ll_data += ll_data_ratio
        forest = new_trees
        appliedEvents = new_appliedevents
        if(ll_data_ratio == 0.0)  print("+")
        else println("ADD EDGE [n_points %d] <%d> [ll_ratio %.3f %.3f %.3f]"
          .format(n_points, time, ll_ratio, ll_points_ratio, ll_data_ratio))
      } else print("!")
    }

    // move to delete contact edge
    {
      val kill = Rand.randInt(n_points).get
      val new_points = points.take(kill-1) ++ points.drop(kill)
      val ll_points_ratio = log( n_points/gamma)

      val (new_trees, new_appliedevents) = BorrowingEvent.treeFromBorrowingEvents(attestedEvents ++ new_points, ntaxa)
      val ll_data_ratio = compute_ll_data_ratio(new_appliedevents, new_trees, ll_data)
      val ll_ratio = ll_points_ratio + ll_data_ratio
      if (log(random) <= (ll_ratio)/temp) { // accept
        points = new_points
        ll_data += ll_data_ratio
        forest = new_trees
        appliedEvents = new_appliedevents
        if(ll_data_ratio == 0.0)  print("-")
        else println("DEL EDGE [n_points %d] <%d> [ll_ratio %.3f %.3f %.3f]"
          .format(n_points, time, ll_ratio, ll_points_ratio, ll_data_ratio))
      }
    }

    // move to move contact edge
    {
      val k = Rand.randInt(n_points).get
      val point = points(k)
      val t = points(k).time
      val new_t = {
        for( s <- Rand.uniform; new_t <- Rand.gaussian(t,tau/(5 * s + 1)))
        yield if(new_t > tau) 2 * tau - new_t
        else if (new_t < 0.0) -new_t
        else new_t
      }.get()
      val new_points = (Set(points:_*) - point + point.copy(time=new_t)).toIndexedSeq
      val ll_points_ratio = 0.0

      val (new_trees, new_appliedevents) = BorrowingEvent.treeFromBorrowingEvents(attestedEvents ++ new_points, ntaxa)
      val ll_data_ratio = compute_ll_data_ratio(new_appliedevents, new_trees, ll_data)
      val ll_ratio = ll_points_ratio + ll_data_ratio
      if (log(random) <= (ll_ratio)/temp) { // accept
        points = new_points.toIndexedSeq
        ll_data += ll_data_ratio
        forest = new_trees
        appliedEvents = new_appliedevents
        if(ll_data_ratio == 0.0)  print("~")
        else println("MOV EDGE [n_points %d] <%d> [ll_ratio %.3f %.3f %.3f]"
          .format(n_points, time, ll_ratio, ll_points_ratio, ll_data_ratio))
      } else print("`")
    }


    lls += (ll_points + ll_data)

  }


}