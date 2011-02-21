package dlwh.cognates
package gaussenstein

import scalanlp.math.Semiring.LogSpace._
import scalanlp.concurrent.ParallelOps._
import scalala.tensor.counters.LogCounters
import scalanlp.fst.{KBest, Alphabet, EditDistance, Automaton}
;


/**
 * 
 * @author dlwh
 */
object AlignmentExperiment {
  def main(args: Array[String]) {
    val dataset = new Dataset("austro_gloss",hasGloss=true);
    val cognates = {for(group <- dataset.cognates)
      yield for (c <- group if c.language == "ProtoOcean" || c.language == "Hawaiian")
      yield c} filter (_.size == 2);

    val wordPairs = for(group <- cognates) yield {
       group.find(_.language == "ProtoOcean").get.word -> group.find(_.language == "Hawaiian").get.word
    }

    val alphabet = (for(pair <- wordPairs iterator; w:String <- pair.productIterator.asInstanceOf[Iterator[String]]; ch <- w iterator) yield ch) toSet
    val charPairs = (for(x <- alphabet + Alphabet[Char].epsilon; y <- alphabet + Alphabet[Char].epsilon)  yield (x,y));
    var trans: scalanlp.fst.Automaton[Double,_,(Char, Char)] = new EditDistance(-4,-5, alphabet);

    val compr = new UniCompression(('#','#')) with NormalizedByFirstChar[Unit,Char];

    type Stats = (LogCounters.LogDoubleCounter[(Char,Char)],Double);
    import scalala.Scalala._;
    def gatherStats(word1: String, word2: String):Stats = {
      val stats = compr.gatherStatistics(charPairs, Automaton.constant(word1,0.0).asTransducer >> trans >> Automaton.constant(word2,0.0).asTransducer );
      stats
    }

    def groupStats (stats1:Stats,stats2:Stats) = {
      (compr.interpolate(stats1._1, 1.0, stats2._1, 1.0),stats1._2 + stats2._2)
    }

    for(i <- 0 until 20) {
      val stats = wordPairs.par.mapReduce({case (w1,w2) => gatherStats(w1,w2)}, groupStats _);
      trans = compr.compress(0.0,stats._1);
      println(stats._2);
    }

    def extract[T:Alphabet](a: Automaton[Double,_, T]) = KBest.extractList(a, 1).head._1.mkString;

    for( (p,c) <- wordPairs) {
      val r = Automaton.constant(p,0.0).asTransducer >> trans >> Automaton.constant(c,0.0).asTransducer
      println(p,c,extract(r), extract(Automaton.constant(p,0.0).asTransducer >> trans outputProjection))
    }


  }
}