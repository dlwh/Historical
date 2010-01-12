package dlwh.cognates;

import scalanlp.counters.LogCounters._;
import scalanlp.fst._;
import scalanlp.util.Log._;
import collection.{mutable=>muta}
import scalanlp.math.Semiring.LogSpace._;

import Types._;

class Fixed(val tree: Tree, factors: TransducerFactors) {
  def inferenceOn(cogs: Seq[Cognate]) = {
    val initialIO = new InsideOutside(tree,factors,Map.empty withDefaultValue Map.empty);
    val io = cogs.foldLeft(initialIO) ( (io,cog) => io.include(cog.language,cog.word,0.0));
    io;
  }

}

object RomanceFixed { 
  def main(arg: Array[String]) {
    globalLog.level = INFO;
    val cognates = Cognates.romance();
    val tree = Tree.romance;

    val alphabet = Set.empty ++ cognates.iterator.flatMap(_.iterator).flatMap(_.word.iterator);
    val factors = new TransducerFactors(tree,alphabet);
    val finalFactors = (1 to 100).foldLeft(factors) { (factors,_) =>
      val fixed = new Fixed(tree, factors);
      
      val ios = for (cogs <- cognates) yield {
        val preTree = tree map { l =>
          val oneBest = cogs.find(_.language == l).map(_.word) getOrElse "<>";
          l + " " + oneBest;
        }
        println(preTree);
        val io = fixed.inferenceOn(cogs)

        val labeledTree = tree map { l =>
          val (oneBest,_) = KBest.extractList(io.marginalFor(l).fsa,1).head;
          l + " " + oneBest.mkString;
        }
        println(cogs);
        println(labeledTree);
        io
      }

      val simpleLearner = new EditDistanceLearner(tree,3, alphabet);
      val transducers = simpleLearner.learnStates(ios);
      new TransducerFactors(tree,alphabet,transducers);
    }
  }
}


