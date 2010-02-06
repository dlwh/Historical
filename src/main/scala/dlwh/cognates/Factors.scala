package dlwh.cognates;

import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;
import scalanlp.math.Numerics._;

import Types._;

trait Factors {
  type Self <: Factors;
  trait EdgeFactorBase {
    def childMarginalize(c: Marginal):Marginal;
    def parentMarginalize(p: Marginal):Marginal;
    def withMarginals(from: Marginal, to: Marginal):EdgeFactor;
  }
  type EdgeFactor <: EdgeFactorBase;

  trait MarginalBase {
    def *(other: Marginal): Marginal
    def partition: Double
    def apply(word: String):Double
  }

  type Marginal <: MarginalBase
  def product(ms: Seq[Marginal]):Marginal;
  def edgeFor(parent: Language, child:Language, alphabet: Set[Char]): EdgeFactor;
  def rootMarginal(alphabet: Set[Char]): Marginal;
  def marginalForWord(w: String, score: Double=0.0): Marginal;
}

trait MarginalPruning { this: TransducerFactors =>
   def prune(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]):Psi
}

trait CompressionPruning extends MarginalPruning { this: TransducerFactors =>
   def compressor(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]):Compressor[_,Char];
   def prune(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]):Psi = {
     val compression = compressor(fsa,length,interestingChars,intBigrams);
     val ret = compression.compress( fsa, fullAlphabet)
     ret;
  }
}

trait PosUniPruning extends CompressionPruning { this: TransducerFactors =>
  def compressor(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]):Compressor[_,Char] = {
    val compression = new PosUniCompression[Char](length+7,'#') with NormalizedTransitions[Int,Char];
    compression
  }
}

trait UniPruning extends CompressionPruning { this: TransducerFactors =>
   def compressor(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]):Compressor[_,Char] = {
    val compression = new SafeUniCompression('#',length + 4);
    compression;
  }
}

/*
trait TriPruning { this: TransducerFactors =>
   def prune(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]) = {
    val compression = new TriCompression[Char](0.0001,15,intBigrams,'#') with NormalizedTransitions[Seq[Char],Char];
    val ret = compression.compress(fsa,interestingChars)
    ret;
  }
}
*/

trait BackedOffKBestPruning { this: TransducerFactors =>
  val numBest = 300;
  def prune(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]) = {
    val kbest : Seq[(Seq[Char],Double)] = KBest.extractList(fsa,numBest);
    val trueCost = fsa.cost;
    val totalCost = logSum(kbest.map(_._2));
    assert(trueCost > totalCost);
    val unigramCost = logDiff(trueCost,totalCost);
    val comp = new UniCompression('#') with NormalizedTransitions[Unit,Char];
    val unigramModel : Automaton[Double,Int,Char] = comp.compress(fsa,interestingChars).scaleInitialWeights( unigramCost - trueCost).relabel;
    import ApproximatePartitioner._;
    val result = kbest.foldLeft(unigramModel) { (fsa,stringscore) =>
      Minimizer.minimize(fsa | Automaton.constant(stringscore._1,stringscore._2))
    }
    result;
  }
}

abstract class TransducerFactors(t: Tree, protected val fullAlphabet: Set[Char],
                        editDistances: Map[(Language,Language),Transducer[Double,_,Char,Char]]=Map.empty) extends Factors with MarginalPruning {
  type Self = TransducerFactors;
  type Marginal = TransducerMarginal;
  type EdgeFactor = TransducerEdge;

  def edgeFor(parent: String, child: String, alphabet: Set[Char]): EdgeFactor = {
    //val ed =  new EditDistance(-5,-6,alphabet,fullAlphabet.size - alphabet.size)
    val ed = (for( ed <- editDistances.get((parent,child)))
              yield ed) getOrElse new EditDistance(-10,-10,fullAlphabet);
    new EdgeFactor(ed,alphabet);
  }

  def rootMarginal(alphabet: Set[Char]) = {
    new Marginal(new DecayAutomaton(8,fullAlphabet) : Psi, 0, Set.empty: Set[Char], Set.empty);
  }

  def product(ms: Seq[Marginal]):Marginal = if(ms.length == 1) ms.head else {
    val inter = ms.map(_.fsa).reduceLeft(_ & _);
    import Minimizer._;
    import ApproximatePartitioner._;
    val minned = minimize(inter.relabel).filterArcs(_.weight != Double.NegativeInfinity);
    val length = ms.map(_.length).max;
    val chars = ms.foldLeft(Set[Char]()) { _ ++ _.interestingChars };
    val bigs = ms.foldLeft(Set[(Char,Char)]()) { _ ++ _.intBigrams };
    val pruned = prune(minned,length, chars, bigs);
    //globalLog.log(INFO)("* out " + memoryString);
    println(pruned);
    new Marginal( pruned,length,chars,bigs);
  }

  def marginalForWord(w: String, score: Double=0.0) = new TransducerMarginal(w,score);

  class TransducerMarginal(val fsa: Psi,
                           val length: Int,
                           val interestingChars: Set[Char],
                           val intBigrams: Set[(Char,(Char))]) extends MarginalBase {
    def this(w: String, cost: Double) = this(Automaton.constant(w,cost), w.length, unigramsOfWord(w), bigramsOfWord(w) );

    /**
    * Computes the product of two marginals by intersecting their automata
    */
    def *(m: Marginal) = product(Seq(this,m));

    /**
    * The log-normalizer of this automata
    */
    lazy val partition = {
      fsa.cost;
    }

    override def toString = { "Marginal 3 Best: " + KBest.extractList(fsa,3)};

    /**
    * returns the log-normalized log probability of the word.
    */
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;
  }

  // parent to child (argument order)
  class TransducerEdge(val fst: Transducer[Double,_,Char,Char], alphabet: Set[Char]) extends EdgeFactorBase {
    def childMarginalize(c: Marginal) = {
      val composed = (fst >> c.fsa.asTransducer).inputProjection;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed.relabel);
      }
      new Marginal(minned, c.length, alphabet, c.intBigrams);
    }
    def parentMarginalize(p: Marginal) = {
      val composed = (p.fsa.asTransducer >> fst).outputProjection.relabel;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed);
      }
      new Marginal(minned, p.length, alphabet, p.intBigrams);
    }

    def withMarginals(from: Marginal, to: Marginal) = {
      new TransducerEdge(from.fsa.asTransducer >> fst >> to.fsa.asTransducer, alphabet);
    }
  }

  private def bigramsOfWord(s: String): Set[(Char,Char)] = {
    Set.empty ++ (s.take(s.length-1) zip s.drop(1));
  }

  private def unigramsOfWord(s: String) = Set.empty ++ s;

  // XXX renormalize edges (maybe, or just let them die)
  private def pruneToAlphabet[S](ed: Transducer[Double,S,Char,Char], validCh: Set[Char]): Transducer[Double,S,Char,Char] = {
    new Transducer[Double,S,Char,Char] {
      def edgesMatching(s: S, ch: (Char,Char)) = {
        ed.edgesMatching(s,ch).filter { case Arc(_,_,(a,b),_) => (validCh(a) && validCh(b)) }
      }
      val initialStateWeights = ed.initialStateWeights;
      def finalWeight(s:S) = ed.finalWeight(s: S);
    }
  }

}
