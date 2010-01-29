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
    val initialIO = new InsideOutside(tree,factors,Map.empty ++ (cogs map (cog => (cog.language,cog.word))));
    initialIO
  }

}

object RomanceFixed { 
  def main(arg: Array[String]) {
    globalLog.level = INFO;
    val cognates = Cognates.romance();
    val tree = Tree.romance;

    val alphabet = Set.empty ++ cognates.iterator.flatMap(_.iterator).flatMap(_.word.iterator);
    val factors = new TransducerFactors(tree,alphabet) with PosUniPruning;
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
          println("Marg for " + l);
          System.out.flush();
          val marg = io.marginalFor(l).get.fsa
          println("Fleshing out " + l);
          System.out.flush();
          val (oneBest,_) = try { KBest.extractList(marg,1).head; } catch { case e => ("<FAIL>".toSeq,Double.NegativeInfinity) }
          l + " " + oneBest.mkString;
        }
        println(cogs);
        System.out.flush();
        println(labeledTree);
        io
      }

      val allPairs = for { 
        a <- alphabet + implicitly[Alphabet[Char]].epsilon;
        b <- alphabet + implicitly[Alphabet[Char]].epsilon
      } yield (a,b);
      
      val edgesToLearn = tree.edges.toSeq;
      val trigramStats = for{
        io <- ios
        (fromL,toL) <- edgesToLearn.iterator
        trans <- io.edgeMarginal(fromL, toL).iterator
      } yield {
        val uRing = new UnigramSemiring[(Char,Char)]( allPairs, ('#','#'), cheatOnEquals= true );
        import uRing._;
        val cost = trans.fst.reweight(promote _, promoteOnlyWeight _ ).cost;

        (fromL,toL) -> cost.decode
      }

      import collection.mutable.{Map=>MMap}
      type APair = (Char,Char)
      val accumulatedStats = MMap[(Language,Language),LogDoubleCounter[(Char,Char)]]();
      for ( (lpair,ctr) <- trigramStats)  {
        val inner = accumulatedStats.getOrElseUpdate(lpair,LogDoubleCounter());
        for( ( t, v) <- ctr) {
          inner(t) = logSum(inner(t),v);
        }
      }

      val tc = new UniCompression[(Char,Char)](allPairs, ('#','#') ) with NormalizedByFirstChar[Unit,Char];
      val transducers = Map.empty ++ accumulatedStats.mapValues ( ctr =>  tc.compress(0.0,ctr));

      new TransducerFactors(tree,alphabet,transducers) with PosUniPruning;
    }
  }
}


