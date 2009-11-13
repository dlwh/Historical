package dlwh.cognates;

import scalanlp.math.Semiring;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;

import Types._;

trait Factors {
  trait EdgeFactorBase {
    def childMarginalize(c: Marginal):Marginal;
    def parentMarginalize(c: Marginal):Marginal;
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
  def edgeFor(parent: String, child: String, alphabet: Set[Char]): EdgeFactor = {
    val ed =  new EditDistance(-5,-6,alphabet,fullAlphabet.size - alphabet.size)
    new EdgeFactor(ed);
  }
  def rootMarginal(alphabet: Set[Char]) = {
    new Marginal(new DecayAutomaton(5.0,alphabet,rhoSize=fullAlphabet.size-alphabet.size));
  }
  def marginalForWord(w: String, score: Double=0.0) = new TransducerMarginal(w,score);

  type Marginal = TransducerMarginal;
  type EdgeFactor = TransducerEdge;

  class TransducerMarginal(val fsa: Psi) extends MarginalBase {
    def this(w: String, cost: Double) = this(Automaton.constant(w,cost));

    /**
    * Computes the product of two marginals by intersecting their automata
    */
    def *(m: Marginal) = {
      val inter = fsa & m.fsa
      import Minimizer._;
      import ApproximatePartitioner._;
      val minned = minimize(inter.inputProjection).relabel;
      new Marginal( minned.relabel.inputProjection);
    }

    def normalize = new Marginal(fsa.pushWeights.inputProjection);
    

    /**
    * The log-normalizer of this automata
    */
    lazy val partition = fsa.cost;

    /**
    * returns the log-normalized log probability of the word.
    */
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;
  }

  // parent to child (argument order)
  class TransducerEdge(val fst: Transducer[Double,_,Char,Char]) extends EdgeFactorBase {
    def childMarginalize(c: Marginal) = {
      println("QQQ compose");
      println(fst);
      println(c.fsa);
      val composed = (fst >> c.fsa).inputProjection;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed).relabel;
      }
      println("XXX " +minned.toConnectivityDot);
      new Marginal(minned.relabel.inputProjection);
    }
    def parentMarginalize(p: Marginal) = {
      val composed = (p.fsa >> fst).outputProjection;
      val minned = {
        import Minimizer._;
        import ApproximatePartitioner._;
        minimize(composed).relabel;
      }
      println("M:"+minned.relabel.cost);
      new Marginal(minned.relabel.outputProjection);
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
