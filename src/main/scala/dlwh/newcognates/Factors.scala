package dlwh.newcognates

/**
 * 
 * @author dlwh
 */
trait Factors {
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