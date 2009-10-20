package dlwh.cognates;

import scalanlp.math.Semiring;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;

import Factors._;
import Types._;

class Factors(t: Tree, marginals: Seq[Language=>Marginal], numGroups: Int, alphabet: Set[Char]) {
  def edgeFor(parent: String, child: String): EdgeFactor = new EdgeFactor(new SimpleEdgeTransducer(-1,-1,alphabet));
  def parentOf(l: String): String = parents(l);
  def expectedScore(psi: Psi, group: Group, language:Language) = {
    val expectedJointScore = Transducer.logSpaceExpectationCompose(marginalFor(group,parentOf(language)).fsa,psi).cost.value;
    val margScore = expectedMarginalScore(group)(language);
    expectedJointScore - margScore;
  }

  def marginalFor(group: Group, ancestor: Language) = marginals(group)(ancestor);

  private def processTree(t: Tree):(Map[Language,Language],Seq[Language]) = t match {
    case Ancestor(label,l,r) => 
      val (lcMap,lcLangs) = processTree(l);
      val (rcMap,rcLangs) = processTree(r);
      val myM = Map(l.label->t.label,r.label->t.label) ++ lcMap ++ rcMap;
      (myM, rcLangs ++ lcLangs);
    case Child(l) => (Map.empty,Seq(l));
  }

  private val (parents,languages) = processTree(t);

  // Maps (Group,Language) => expected edge cost for any child.
  val expectedMarginalScore: Seq[Map[Language,Double]] = {
    for ( groupMarginals <- marginals) 
    yield Map.empty ++ { for( language <- languages)
      yield { 
        val parent = parentOf(language);
        val marg = groupMarginals(parent);
        val parentMarginal = edgeFor(parent,language).fst.inputProjection;
        val marginalCost = Transducer.logSpaceExpectationCompose(marg.fsa,parentMarginal).cost.value;
        (language,marginalCost);
      }
    }
  }
}

object Factors {
  class Marginal(val fsa: Psi) {
    /**
    * Computes the product of two marginals by intersecting their automata
    */
    def *(m: Marginal) = {
      new Marginal(this.fsa&m.fsa);
    }

    /**
    * The log-normalizer of this automata
    */
    lazy val partition = fsa.cost;

    /**
    * returns the log-normalized log probability of the word.
    */
    def apply(w: String)= (fsa & Automaton.constant(w,0.0)).cost - partition;
  }

  // parent to child (argument order)
  class EdgeFactor(val fst: Transducer[Double,_,Char,Char]) {
    def childMarginalize(c: Marginal) = {
      new Marginal((c.fsa >> fst.swapLabels).outputProjection);
    }
    def parentMarginalize(p: Marginal) = {
      new Marginal((p.fsa >> fst).outputProjection);
    }
  }

  /**
  * Levhenstein distance with the given parameters, which must be less than 0.
  */
  class SimpleEdgeTransducer(sub: Double, del: Double, alphabet: Set[Char]) 
      extends Transducer[Double,Unit,Char,Char] {
    import Transducer._;
    require( sub < 0);
    require( del < 0);

    implicit val ring = implicitly[Semiring[Double]];

    val initialStateWeights = Map( () -> 0.0);

    def finalWeight(s: Unit) = 0.0;

    override val allEdges = {
      val subs = {for(a <- alphabet iterator;
          b <- alphabet iterator)
        yield Arc((),(),Some(a),Some(b),if(a != b) sub else 0.0)}.toSeq;
      val dels = for(a <- alphabet iterator)
        yield Arc((),(),Some(a),None,del);
      val dels2 = for(a <- alphabet iterator)
        yield Arc((),(),None,Some(a),del);

      subs ++ dels ++ dels2;
    }

    def edgesFrom(s: Unit) = allEdges;

    override def edgesWithOutput(s: Unit, a: Option[Char]) = a match {
      case Some(_) =>
        (for(b <- alphabet iterator)
          yield Arc((),(),Some(b),a,if(a != b) sub else 0.))toSeq;
      case None =>
        (for(a <- alphabet iterator)
          yield Arc((),(),Some(a),None,del)) toSeq;
    }

    override def edgesWithInput(s: Unit, a: Option[Char]) = a match {
      case sa@ Some(a) =>
        for(b <- alphabet toSeq)
          yield Arc((),(),sa,Some(b),if(a != b) sub else 0.);
      case None =>
        for(a <- alphabet toSeq)
          yield Arc((),(),None,Some(a),del);
    }

  }
}
