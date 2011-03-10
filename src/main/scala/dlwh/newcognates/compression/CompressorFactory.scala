package dlwh.newcognates.compression

import dlwh.newcognates.FastTransducerFactory
import scalala.tensor.sparse.{SparseVector, SparseHashVector}
import scalanlp.collection.mutable.SparseArray
import scalala.Scalala._;

/**
 * 
 * @author dlwh
 */

trait CompressorFactory  { this : FastTransducerFactory =>
  import this.factory._;
  val model : Automaton;
  def compress(a: Automaton) = {
    // counts -> char -> destination
    val (counts: Array[Array[SparseVector]],totals,finalWeights) = expectedCounts(a,model)
    val arcs = counts.zipWithIndex.map { case (c,state) =>
      val r = encoder.fillSparseArray(mkSparseVector(model.numStates))
      for( (dests,ch) <- c.zipWithIndex) {
        if(totals(state) != Double.NegativeInfinity)
          dests -= totals(state);
        r(ch) = dests;
      }
      r
    }
    val outWeights = Array.tabulate(model.numStates) { i =>
      if(totals(i) == Double.NegativeInfinity) Double.NegativeInfinity
      else finalWeights(i) - totals(i)
    }
    automaton(arcs, outWeights, model.initialState, a.cost);
  }
}