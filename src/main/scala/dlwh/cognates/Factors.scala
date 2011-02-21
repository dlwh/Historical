package dlwh.cognates;

import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;
import scalanlp.math.Numerics._;

import Types._;

/**
 * Factors handles what to do with the marginals and messages as we're passing them.
 */
trait Factors {

  /**
   * Required interface for EdgeFactors
   */
  trait EdgeFactorBase {
    def childMarginalize(c: Marginal):Marginal;
    def parentMarginalize(p: Marginal):Marginal;
    def withMarginals(from: Marginal, to: Marginal):EdgeFactor;
  }
  type EdgeFactor <: EdgeFactorBase;

  /**
   * Required interface for Marginals
   */
  trait MarginalBase {
    def partition: Double
    def apply(word: String):Double
  }
  type Marginal <: MarginalBase

  def product(upward: Boolean, ms: Seq[Marginal]):Marginal;
  def edgeFor(parent: Language, child:Language, alphabet: Set[Char]): EdgeFactor;
  def rootMarginal(alphabet: Set[Char]): Marginal;
  def marginalForWord(w: String, score: Double=0.0): Marginal;
}

class TransducerFactors(t: Tree,
                        fullAlphabet: Set[Char],
                        compression: MessageCompressor[_],
                        editDistances: Map[(Language,Language),Transducer[Double,_,Char,Char]]=Map.empty,
                        root: Option[Psi]=None) extends Factors {
  type Marginal = TransducerMarginal;
  type EdgeFactor = TransducerEdge;

  def edgeFor(parent: String, child: String, alphabet: Set[Char]): EdgeFactor = {
    //val ed =  new EditDistance(-5,-6,alphabet,fullAlphabet.size - alphabet.size)
    val ed = (for( ed <- editDistances.get((parent,child)))
              yield ed) getOrElse new EditDistance(-5E-10,-4E-10,fullAlphabet);
    new EdgeFactor(ed);
  }

  def rootMarginal(alphabet: Set[Char]) = {
    new Marginal(root.getOrElse(new DecayAutomaton(8,fullAlphabet)) : Psi, 0);
  }

  private val upwardCompressor = new SafeCompression(fullAlphabet,compression,8);

  def product(upward: Boolean, ms: Seq[Marginal]):Marginal = {
    val inter = ms.map(_.fsa).reduceLeft(_ & _);
    import Minimizer._;
    import ApproximatePartitioner._;
    val minned = minimize(inter.relabel).filterArcs(_.weight != Double.NegativeInfinity);
    val length = ms.map(_.length).max;
    val pruned = (
      if(upward) upwardCompressor.compress(minned,fullAlphabet)
      else compression.compress(minned,fullAlphabet)
      );
    //globalLog.log(INFO)("* out " + memoryString);
    new Marginal( pruned,length);
  }

  def marginalForWord(w: String, score: Double=0.0) = new TransducerMarginal(w,score);

  class TransducerMarginal(val fsa: Psi, val length: Int) extends MarginalBase {
    def this(w: String, cost: Double) = this(Automaton.constant(w,cost), w.length);

    /**
    * The log-normalizer of this automata
    */
    lazy val partition = {
      fsa.cost;
    }

    override def toString = { "Marginal 3 Best: " + fsa };

    /**
    * returns the log probability of the word.
    */
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;
  }

  // parent to child (argument order)
  class TransducerEdge(val fst: Transducer[Double,_,Char,Char]) extends EdgeFactorBase {
    def childMarginalize(c: Marginal) = {
      val composed = (fst >> c.fsa.asTransducer).inputProjection;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed.relabel);
      }
      new Marginal(minned, c.length);
    }
    def parentMarginalize(p: Marginal) = {
      val composed = (p.fsa.asTransducer >> fst).outputProjection.relabel;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed);
      }
      new Marginal(minned, p.length);
    }

    def withMarginals(from: Marginal, to: Marginal) = {
      new TransducerEdge(from.fsa.asTransducer >> fst >> to.fsa.asTransducer);
    }
  }

}
