package dlwh.cognates;

import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;

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

  def edgeFor(parent: Language, child:Language, alphabet: Set[Char]): EdgeFactor;
  def rootMarginal(alphabet: Set[Char]): Marginal;
  def marginalForWord(w: String, score: Double=0.0): Marginal;
}

class TransducerFactors(t: Tree, fullAlphabet: Set[Char],
                        editDistances: Map[(Language,Language),Transducer[Double,_,Char,Char]]=Map.empty) extends Factors {
  type Self = TransducerFactors;
  type Marginal = TransducerMarginal;
  type EdgeFactor = TransducerEdge;

  def edgeFor(parent: String, child: String, alphabet: Set[Char]): EdgeFactor = {
    //val ed =  new EditDistance(-5,-6,alphabet,fullAlphabet.size - alphabet.size)
    val ed = (for( ed <- editDistances.get((parent,child)))
              yield pruneToAlphabet(ed,alphabet)) getOrElse new EditDistance(-4,-4,alphabet);
    new EdgeFactor(ed,alphabet);
  }

  def rootMarginal(alphabet: Set[Char]) = {
    new Marginal(new DecayAutomaton(5.0,alphabet) : Psi, 0, Set.empty: Set[Char], Set.empty);
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
    def *(m: Marginal) = {
      val inter = fsa & m.fsa
      import Minimizer._;
      import ApproximatePartitioner._;
      val minned = minimize(inter.relabel);
      val pruned = prune(minned,length max m.length,this.interestingChars ++ m.interestingChars,this.intBigrams ++ m.intBigrams);
      new Marginal( pruned,length max m.length,this.interestingChars ++ m.interestingChars, this.intBigrams ++ m.intBigrams);
    }

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

  def prune(fsa: Psi, length: Int, interestingChars: Set[Char], intBigrams: Set[(Char,Char)]) = {
    val str = "Pre-Pruned 3 Best: " + KBest.extractList(fsa,3);
    println(str);
    //val compression = new TriCompression[Char](-100.0,15,interestingChars,intBigrams,'#');
    val compression = new PosUniCompression[Char](length+5,interestingChars,'#');
    val ret = compression.compress(fsa)
    ret;
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

  private def processTree(t: Tree):(Map[Language,Language],Seq[Language]) = t match {
    case Ancestor(label,l,r) =>
      val (lcMap,lcLangs) = processTree(l);
      val (rcMap,rcLangs) = processTree(r);
      val myM = Map(l.label->t.label,r.label->t.label) ++ lcMap ++ rcMap;
      (myM, rcLangs ++ lcLangs);
    case Child(l) => (Map.empty,Seq(l));
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

  private val (parents,languages) = processTree(t);
}
