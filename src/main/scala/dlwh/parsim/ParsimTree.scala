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

object ParsimTree extends App {
  val configuration = Configuration.fromPropertiesFiles(args.map(new File(_)))
  val dataset = Dataset.fromConfiguration(configuration)
  val goldTree = dataset.tree
  println(goldTree)
  val leaves = goldTree.leaves.toSet
  val cognates = dataset.cognates.flatten.filter(c => leaves(c.language));
  val langIndex = Index(dataset.languages.filter(leaves))
  val groupedCognates: IndexedSeq[Map[_root_.dlwh.cognates.Language, IndexedSeq[Cognate]]] = groupCognates(cognates)
  val gold = new CognatesEval(dataset.cognates.map(_.filter(c => leaves(c.language))));


  val ntaxa = langIndex.size

  val charIndex = Index(cognates.flatMap(_.word) ++  Iterator('\0'))

  // parameters
  val tau = 10000.0 // oldest time
  val gamma0 = 5E-6

  val process = new PPTree(ntaxa, tau, gamma0, IndexedSeq.tabulate(ntaxa)(n => new AttestationEvent(n, 0.0)) ++ IndexedSeq(OriginEvent(10000)))

  type T = Set[Language]

  def initWeight(x: Int) = -3.
  def transRatio(x: Int, b: Int) = if(x == b) 0 else -4
  val ed = new GeneralEditDistance(1, charIndex, initWeight, initWeight, transRatio)
  val factorsFactory = new InnovationFactorsFactory(new WordFactorsFactory(ed),
                                                    new LanguageModelFactorsFactory(charIndex), 0.2)


  var decoded:IndexedSeq[IndexedSeq[Cognate]] = null
  var params = Map.empty[T,factorsFactory.EdgeParameters] withDefaultValue factorsFactory.initialParameters;
  def likelihood(tree: Tree[Int]):Double = {
    val annTree = tree.map(i => if(i >= 0) langIndex.get(i) else "").extend(_.leaves.toSet)
    def cognateFor(group: Map[Language,IndexedSeq[Cognate]], lang: Set[Language]) = {
      if(lang.size == 1) group.get(lang.iterator.next).getOrElse(Seq.empty)
      else Seq.empty
    }

    var likelihood = -1E10
    while(true) {
      val inference: IndexedSeq[TreeInference[T,factorsFactory.Factors[T]]] = for {
        c <- groupedCognates
        if c.keys.exists(annTree.label)
        factors = factorsFactory.factorsFor(c.values.flatMap(_.map(_.word)).toSet,params)
        inf = new TreeInference(factors, annTree, { (x:T) => cognateFor(c, x)})
      } yield {
        inf
      }
      assert(!inference.isEmpty,annTree)
      val newLikelihood : Double = inference.par.map(_.likelihood).sum
      val change = (newLikelihood - likelihood) / newLikelihood.abs
      println("::" + newLikelihood + ",  " + change)
      if( change < 1E-4) {
        println("...")
        return newLikelihood
      }

      decoded = inference.par.flatMap(decodeCognates(_,annTree)).seq.toIndexedSeq
      val (p,r)  = gold.precisionAndRecall(decoded.map(g => CognateGroup(g:_*)))
      val f1 = 2 * p * r / (p + r)

      println(newLikelihood,likelihood,change,p,r,f1)

      likelihood = newLikelihood
      val ecounts = inference.par.map(_.sufficientStatistics).reduceLeft(sumStats _)
      params = factorsFactory.optimize(ecounts)
    }
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
