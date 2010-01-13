package dlwh.cognates;

import scalanlp.counters.LogCounters._;
import scalanlp.fst._;
import scalanlp.util.Log._;
import collection.{mutable=>muta}
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.math.Numerics._;

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
      
      val ios = for (cogs <- cognates.iterator) yield {
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

      val edgesToLearn = tree.edges.toSeq;
      val trigramStats = for{
        io <- ios
        (fromL,toL) <- edgesToLearn.iterator
      } yield {
        val ring = new TrigramSemiring[(Char,Char)]( alphabet.map { a => (a,a)}, Set.empty, ('#','#') );
        import ring._;
        val trans = io.edgeMarginal(fromL, toL);
        val cost = trans.fst.reweight(promote _, promoteOnlyWeight _ ).cost;

        val ctr = LogPairedDoubleCounter[(Char,Char),(Char,Char)]();
        for( (k1,k2,v) <- cost.decodeBigrams.triples) {
          ctr(k1,k2) = logSum(ctr(k1,k2),v);
        }
        (fromL,toL) -> ctr
      }

      import collection.mutable.{Map=>MMap}
      type APair = (Char,Char)
      val accumulatedStats = MMap[(Language,Language),LogPairedDoubleCounter[APair,APair]]();
      for ( (lpair,ctr) <- trigramStats)  {
        val inner = accumulatedStats.getOrElseUpdate(lpair,LogPairedDoubleCounter());
        for( (k1,k2,v) <- ctr.triples) {
          inner(k1,k2) = logSum(inner(k1,k2),v);
        }
      }

      import TrigramSemiring._;
      val trigrams = LogPairedDoubleCounter[Bigram[(Char,Char)],(Char,Char)]();
      val tc = new TriCompression[(Char,Char)](1.0, 5,alphabet.map { a => (a,a)}, Set.empty, ('#','#') ) with NormalizedByFirstChar[Seq[(Char,Char)],Char];
      // This isn't right.
      val transducers = Map.empty ++ accumulatedStats.mapValues ( ctr =>  tc.compress(0.0,trigrams,ctr));

      new TransducerFactors(tree,alphabet,transducers);
    }
  }
}


