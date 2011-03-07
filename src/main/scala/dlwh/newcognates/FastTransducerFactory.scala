package dlwh.newcognates

import scalanlp.fst.fast.AutomatonFactory
import scalanlp.util.Index
import scalanlp.fst.Alphabet

class FastTransducerFactory(alphabet: Set[Char]) {
  val factory = new AutomatonFactory[Char](Index(alphabet + implicitly[Alphabet[Char]].epsilon));
  import factory._;

  /**
   *
   * @author dlwh
   */
  class FastTransducerFactors(factory: AutomatonFactory[Char],
                              rootBelief: factory.Automaton
                                editDistance) extends Factors {
    def initialBelief(lang: Language) = null

    def initialMessage(from: Language, to: Language) = null

    def beliefForWord(w: Word) = null

    def rootMessage = null

    def edgeFor(parent: Language, child: Language) = null

    type Edge = this.type
    type Belief = this.type
  }
}

