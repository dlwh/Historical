package dlwh.newcognates

import compression.CompressorFactory
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst.fast.AutomatonFactory
import scalanlp.util.Index
import scalanlp.fst.Alphabet

abstract class FastTransducerFactory extends CompressorFactory {
  val factory:AutomatonFactory[Char];
  import factory._;

  /**
   *
   * @author dlwh
   */
  class FastTransducerFactors(rootBelief: Automaton,
                              baseBelief: (Language)=>Automaton,
                              editDistance: (Language,Language)=>Transducer,
                              initMessage: (Language,Language)=>Automaton) extends Factors {
    def initialBelief(lang: Language) = new Belief(baseBelief(lang));

    def initialMessage(from: Language, to: Language) = new Belief(initMessage(from,to));

    def beliefForWord(w: Word) = new Belief(constant(w,0.0));

    val rootMessage = new Belief(rootBelief);

    def edgeFor(parent: Language, child: Language) = new Edge(editDistance(parent,child));

    case class Belief(fsa: Automaton) extends BeliefBase {
      lazy val partition = fsa.cost;
      def apply(word: String)= (fsa & constant(word,0.0)).cost - partition;

      def /(b: Belief):Belief = {
        val newFsa = (fsa & b.fsa.transformWeights(-1.0 * _))
        new Belief(newFsa);
      }

      def *(b: Belief):Belief = {
        new Belief(fsa & b.fsa);
      }

      override def toString() = ("Belief: " + fsa.toString);

      def normalized = scaleBy(-partition)

      def scaleBy(score: Double) = {
        val scaled = fsa.scaleInitialWeight(score)
        new Belief(scaled);
      }

    }

    class Edge(val trans: Transducer) extends EdgeBase {
      def edgeMarginal(parent: Belief, child: Belief):Edge = {
        new Edge(parent.fsa.asTransducer >> trans >> child.fsa.asTransducer)
      }

      def parentProjection:Belief = {
        val parent = compress(trans.inputProjection)
        new Belief(parent)
      }

      def childProjection:Belief = {
        val child = compress(trans.outputProjection)
        new Belief(child)
      }
    }
  }
}

