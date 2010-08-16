package dlwh.cognates

import scalanlp.concurrent.ParallelOps._;
import scala.collection.mutable.PriorityQueue;
import Bipartite._;

import Types._;

class Agglomerative[F<:Factors](val tree: Tree,
                                languages: Seq[Language],
                                treePenalty: Double,
                                initDeathProb: Double,
                                factorBroker: FactorBroker[F]) {

  def initialState(words: IndexedSeq[Cognate]) = {
    val groups = words.map(c => Map(c.language -> c));
    State(groups, factorBroker.initialFactors, Map.empty.withDefaultValue(initDeathProb));
  }
  
  def step(state: State[F], numGroupings: Int = 100) = {
    val affinities = computeInitialAffinities(state);
  }

  def computeInitialAffinities(state: State[F]) = {

    val ios: IndexedSeq[InsideOutside[F]] = state.groups.par.map{g =>
      val io = state.makeIO(tree,g);
      println(g);
      io.likelihood; // precompute likelihood
      io
    };

    val scores: IndexedSeq[(InsideOutside[F],InsideOutside[F], Double)] = for {
      g1Index <- ios.zipWithIndex.par;
      g2 <- ios.view(g1Index._2 + 1,ios.length)
      score <- mergeScore(g1Index._1,g2)
    } yield (g1Index._1,g2,score);

    val pq = new PriorityQueue[(InsideOutside[F],InsideOutside[F], Double)]()(Ordering[Double].on(_._3));
    pq ++= scores
    pq
  }

  private def mergeScore(gi: InsideOutside[F], gj: InsideOutside[F]) = {
    if(gi.words.first._2.gloss != gj.words.first._2.gloss) None
    else for( merged <- gi merge gj)
      yield (merged.likelihood - gi.likelihood - gj.likelihood);
  }

}