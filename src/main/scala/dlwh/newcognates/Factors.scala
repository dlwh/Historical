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
  }

  type Belief <: BeliefBase

  def sendMessageToChild(message: Belief, parent: Language, child: Language):Belief
  def sendMessageToParent(message: Belief, child: Language, parent: Language):Belief
  def rootMessage: Belief;
  def product(messages: Seq[Belief]):Belief
  def beliefForWord(w: Word): Belief
  def initialBelief(l: Language):Belief
}


/**
 * Factors where all transducers are projected to bigrams
 */
class BigramFactors extends Factors {
  def sendMessageToChild(message: Belief, parent: Language, child: Language):Belief = message;
  def sendMessageToParent(message: Belief, child: Language, parent: Language):Belief = message;
  val rootMessage: Belief = new Belief(Set.empty)
  def product(messages: Seq[Belief]) = new Belief(messages.foldLeft(Set.empty[(Char,Char)])(_ ++ _.bigrams));

  def beliefForWord(w: Word) = Belief(extractBigrams(w));
  def initialBelief(l: Language) = rootMessage;

  case class Belief(bigrams: Set[(Char,Char)]) extends BeliefBase {
    def apply(w: String) = {
      val wB = extractBigrams(w);
      2.0 * (wB & this.bigrams size) / (wB.size + bigrams.size);
    }

    def partition = bigrams.size toDouble;
  }

  def extractBigrams(word: Word) = {
    Set.empty ++ (word.take(word.length-1) zip word.drop(1))
  }
}

class TransducerFactors(fullAlphabet: Set[Char],
                        compression: MessageCompressor[_],
                        rootBelief: Psi,
                        editDistance: (Language,Language)=>Transducer[Double,_,Char,Char],
                        initBelief: (Language)=>Psi) extends Factors {

  class Belief(val fsa: Psi, val length: Int) extends BeliefBase {
    def partition = fsa.cost;
    def apply(word: String)= (fsa & Automaton.constant(word,0.0)).relabel.cost - partition;
  }

  def sendMessageToChild(message: Belief, parent: Language, child: Language):Belief = {
    val newFsa = (message.fsa.asTransducer >> editDistance(parent,child)).outputProjection.relabel
    new Belief(newFsa,message.length);
  }

  def sendMessageToParent(message: Belief, child: Language, parent: Language):Belief = {
    val newFsa = (editDistance(parent,child)>> message.fsa.asTransducer).inputProjection.relabel
    new Belief(newFsa,message.length);
  }

  val rootMessage: Belief = new Belief(rootBelief,5);
  def product(messages: Seq[Belief]):Belief = {
    val inter = messages.view.map(_.fsa).reduceLeft(_ & _);
    import Minimizer._;
    import ApproximatePartitioner._;
    val minned = minimize(inter.relabel).filterArcs(_.weight != Double.NegativeInfinity);
    val length = messages.map(_.length).max;
    val pruned = (
      compression.compress(minned,fullAlphabet)
    );
    new Belief( pruned,length);
  }
  def beliefForWord(w: Word): Belief = new Belief(Automaton.constant(w,0.0),w.size);
  def initialBelief(l: Language):Belief = new Belief(initBelief(l),0);
}