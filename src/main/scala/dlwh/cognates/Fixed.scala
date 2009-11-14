package dlwh.cognates;

import scalanlp.counters.Counters.PairedDoubleCounter;
import scalanlp.counters.Counters.DoubleCounter;
import scalanlp.counters.LogCounters._;
import scalanlp.stats.sampling.Multinomial;
import scalanlp.fst._;
import scalanlp.util.Log._;
import collection.{mutable=>muta}
import scalanlp.math.Semiring.LogSpace._;

import Types._;

class Fixed(val tree: Tree) {
  def inferenceOn(cogs: Seq[Cognate]) = {
    val alphabet = Set.empty ++ cogs.iterator.flatMap(_.word.iterator);
    val factors = new TransducerFactors(tree,alphabet);
    val initialIO = new InsideOutside(tree,factors,Map.empty withDefaultValue Map.empty);
    val io = cogs.foldLeft(initialIO) ( (io,cog) => io.include(cog.language,cog.word,0.0));
    (factors,io);
  }

}

object RomanceFixed { 
  def main(arg: Array[String]) {
    globalLog.level = INFO;
    val cognates = Cognates.romance();
    val tree = Tree.romance;

    val fixed = new Fixed(tree);
    for{
      cogs <- cognates;
      (factors,io) = fixed.inferenceOn(cogs)
    } {
      val labeledTree = tree map { l =>
        val (oneBest,_) = KBest.extractList(io.marginalFor(l).fsa,1).head;
        l + " " + oneBest.mkString;
      }
      println(cogs);
      println(labeledTree);
    }
  }
}


