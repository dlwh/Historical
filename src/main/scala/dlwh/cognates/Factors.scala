package dlwh.cognates;

import scalanlp.math.Semiring;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;

import Types._;

trait Factors {
  type Self <: Factors;
  trait EdgeFactorBase {
    def childMarginalize(c: Marginal):Marginal;
    def parentMarginalize(p: Marginal):Marginal;
  }
  type EdgeFactor <: EdgeFactorBase;

  trait MarginalBase {
    def *(other: Marginal): Marginal
    def partition: Double
    def apply(word: String):Double
  }

  type Marginal <: MarginalBase

  def edgeFor(parent: Language, child:Language, alphabet: Set[Char]): EdgeFactor;
  def rootMarginal(alphabet: Set[Char]): Marginal;
  def marginalForWord(w: String, score: Double=0.0): Marginal;
}

class TransducerFactors(t: Tree, fullAlphabet: Set[Char]) extends Factors {
  type Self = TransducerFactors;
  def edgeFor(parent: String, child: String, alphabet: Set[Char]): EdgeFactor = {
    //val ed =  new EditDistance(-5,-6,alphabet,fullAlphabet.size - alphabet.size)
    val ed =  new EditDistance(-5,-6,fullAlphabet)
    new EdgeFactor(ed);
  }
  def rootMarginal(alphabet: Set[Char]) = {
    new Marginal(new DecayAutomaton(5.0,fullAlphabet) : Psi, Set.empty: Set[(Char,Char)]);
  }
  def marginalForWord(w: String, score: Double=0.0) = new TransducerMarginal(w,score);

  type Marginal = TransducerMarginal;
  type EdgeFactor = TransducerEdge;

  class TransducerMarginal(val fsa: Psi, val interestingChars: Set[(Char,Char)]) extends MarginalBase {
    def this(w: String, cost: Double) = this(Automaton.constant(w,cost), Set.empty ++ w.map(c => (c,c)));

    /**
    * Computes the product of two marginals by intersecting their automata
    */
    def *(m: Marginal) = {
      val inter = fsa & m.fsa
      import Minimizer._;
      import ApproximatePartitioner._;
      val minned = minimize(inter.relabel).inputProjection;
      val pruned = prune(minned,this.interestingChars ++ m.interestingChars);
      new Marginal( pruned,this.interestingChars ++ m.interestingChars);
    }

    def normalize = new Marginal(fsa.pushWeights.inputProjection,interestingChars);
    

    /**
    * The log-normalizer of this automata
    */
    lazy val partition = {
      fsa.cost;
    }

    /**
    * returns the log-normalized log probability of the word.
    */
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;
  }

  def prune(fsa: Psi, interestingChars: Set[(Char,Char)]) = {
    val compression = new TriCompression(0.01,20,interestingChars);
    compression.compress(fsa).inputProjection
  }

  // parent to child (argument order)
  class TransducerEdge(val fst: Transducer[Double,_,Char,Char]) extends EdgeFactorBase {
    def childMarginalize(c: Marginal) = {
      val composed = (fst >> c.fsa).inputProjection;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed.relabel).inputProjection;
      }
      // XXX todo
      new Marginal(minned, c.interestingChars);
    }
    def parentMarginalize(p: Marginal) = {
      val composed = (p.fsa >> fst).outputProjection.relabel;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed).outputProjection;
      }
      new Marginal(minned, p.interestingChars);
    }
  }

  private def processTree(t: Tree):(Map[Language,Language],Seq[Language]) = t match {
    case Ancestor(label,l,r) =>
      val (lcMap,lcLangs) = processTree(l);
      val (rcMap,rcLangs) = processTree(r);
      val myM = Map(l.label->t.label,r.label->t.label) ++ lcMap ++ rcMap;
      (myM, rcLangs ++ lcLangs);
    case Child(l) => (Map.empty,Seq(l));
  }

  private val (parents,languages) = processTree(t);
}
