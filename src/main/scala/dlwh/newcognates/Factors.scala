package dlwh.newcognates

import scalanlp.fst._;
import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace;

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
    def *(b: Belief):Belief;
  }

  type Belief <: BeliefBase
  type Edge <: EdgeBase

  trait EdgeBase {
    def edgeMarginal(parent: Belief, child: Belief):Edge
    // compute the "product" (parent * e * child), and compute the resulting posteriors for the child
    def childProjection:Belief
    // compute the "product" (parent * e * child), and compute the resulting posteriors for the child
    def parentProjection:Belief
  }

  def edgeFor(parent: Language, child: Language):Edge

  def rootMessage: Belief;
  def beliefForWord(w: Word): Belief
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

    def partition = 1.0;

    def /(b: Belief) = this;
    def *(b: Belief) = Belief(this.bigrams ++ b.bigrams);
  }

  class Edge(beliefs: Belief*) extends EdgeBase {
    def edgeMarginal(parent: Belief, child: Belief) = new Edge(parent,child);
    def childProjection:Belief = Belief(beliefs.view.map(_.bigrams).reduceLeft(_++_));
    def parentProjection:Belief = childProjection;
  }

  def extractBigrams(word: Word) = {
    Set.empty ++ (word.take(word.length-1) zip word.drop(1))
  }
}

class TransducerFactors(fullAlphabet: Set[Char],
                        compression: MessageCompressor[_],
                        rootBelief: Psi,
                        editDistance: (Language,Language)=>Transducer[Double,_,Char,Char],
                        initMessage: (Language,Language)=>Psi) extends Factors {

  case class Belief(fsa: Psi) extends BeliefBase {
    lazy val partition = fsa.cost;
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;

    def /(b: Belief):Belief = {
      val newFsa = fsa & b.fsa.reweight(- _.weight, - _).relabel;
      new Belief(newFsa);
    }

    def *(b: Belief):Belief = {
      new Belief(fsa & b.fsa relabel);
    }

    override def toString() = ("Belief: " + fsa.toString);
  }

  class Edge(val trans: Transducer[Double,_,Char,Char]) extends EdgeBase {
    def edgeMarginal(parent: Belief, child: Belief):Edge = {
      new Edge(parent.fsa.asTransducer >> trans >> child.fsa.asTransducer)
    }

    def parentProjection:Belief = {
      val parent = compression.compress(trans.inputProjection, fullAlphabet)
      new Belief(parent)
    }

    def childProjection:Belief = {
      val child = compression.compress(trans.outputProjection, fullAlphabet)
      new Belief(child)
    }
  }

  val rootMessage: Belief = new Belief(rootBelief);
  def edgeFor(a: Language, b: Language) = new Edge(editDistance(a,b));
  def beliefForWord(w: Word): Belief = new Belief(Automaton.constant(w,0.0));
  def initialMessage(from: Language, to: Language):Belief = new Belief(initMessage(from,to));
}

