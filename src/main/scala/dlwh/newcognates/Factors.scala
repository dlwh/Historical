package dlwh.newcognates

import scalanlp.fst._;
import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace;
import scalanlp.util.Lazy;

/**
 * 
 * @author dlwh
 */
trait Factors {
  final val ROOT = "<ROOT>";
  trait BeliefBase {
    def apply(w: Word):Double
    def partition: Double
    def /(b: Belief):Belief;
  }

  type Belief <: BeliefBase
  type Edge <: EdgeBase

  trait EdgeBase {
  }

  // compute the "product" (parent * e * child), and compute the resulting posteriors for parent and child
  def projectMessages(parentBelief: Belief, childBelief: Belief, e: Edge): (Lazy[Belief],Lazy[Belief])
  def edgeFor(parent: Language, child: Language):Edge

  def rootMessage: Belief;
  def beliefForWord(w: Word): Belief
  def initialBelief(l: Language):Belief
  def initialMessage(from: Language, to: Language): Belief
}


/**
 * Factors where all transducers are projected to bigrams
 */
class BigramFactors extends Factors {
  val rootMessage: Belief = new Belief(Set.empty)
  def beliefForWord(w: Word) = Belief(extractBigrams(w));
  def initialBelief(l: Language) = rootMessage;
  def initialMessage(from: Language, to: Language): Belief = rootMessage;
  def edgeFor(l: Language, child: Language) = new Edge()

  case class Belief(bigrams: Set[(Char,Char)]) extends BeliefBase {
    def apply(w: String) = {
      val wB = extractBigrams(w);
      2.0 * (wB & this.bigrams size) / (wB.size + bigrams.size);
    }

    def partition = bigrams.size toDouble;

    def /(b: Belief) = this;
  }

  class Edge extends EdgeBase

  def projectMessages(parentBelief: Belief, childBelief: Belief, e: Edge) = {
    val union = Lazy(Belief(parentBelief.bigrams ++ childBelief.bigrams));
    (union,union)
  }

  def extractBigrams(word: Word) = {
    Set.empty ++ (word.take(word.length-1) zip word.drop(1))
  }
}

class TransducerFactors(fullAlphabet: Set[Char],
                        compression: MessageCompressor[_],
                        rootBelief: Psi,
                        editDistance: (Language,Language)=>Transducer[Double,_,Char,Char],
                        initBelief: (Language)=>Psi,
                        initMessage: (Language,Language)=>Psi) extends Factors {

  class Belief(val fsa: Psi, val length: Int) extends BeliefBase {
    def partition = fsa.cost;
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;

    def /(b: Belief):Belief = {
      val newFsa = fsa & fsa.reweight(- _.weight, - _).relabel;
      new Belief(newFsa, length max b.length);
    }
  }

  class Edge(val trans: Transducer[Double,_,Char,Char]) extends EdgeBase {
  }

  def projectMessages(parentBelief: Belief, childBelief: Belief, e: Edge) = {
    lazy val edgeMarginal = parentBelief.fsa.asTransducer >> e.trans >> childBelief.fsa.asTransducer;
    lazy val parent = compression.compress(edgeMarginal.inputProjection, fullAlphabet)
    lazy val child = compression.compress(edgeMarginal.outputProjection, fullAlphabet)
    val length = parentBelief.length max childBelief.length;
    (Lazy(new Belief(parent,length)),Lazy(new Belief(child, length)));
  }

  val rootMessage: Belief = new Belief(rootBelief,0);
  def edgeFor(a: Language, b: Language) = new Edge(editDistance(a,b));
  def beliefForWord(w: Word): Belief = new Belief(Automaton.constant(w,0.0),w.size);
  def initialBelief(l: Language):Belief = new Belief(initBelief(l),0);
  def initialMessage(from: Language, to: Language):Belief = new Belief(initMessage(from,to),0);
}

