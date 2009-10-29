package dlwh.cognates;

import scalanlp.math.Semiring;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;

import Factors._;
import Types._;

class Factors(t: Tree, marginals: Seq[Language=>Marginal], numGroups: Int, alphabet: Set[Char]) {
  def edgeFor(parent: String, child: String): EdgeFactor = ef;
  val ef = new EdgeFactor(new SimpleEdgeTransducer(-1,-1,alphabet));
  def parentOf(l: String): String = parents(l);
  def expectedScore(psi: Psi, group: Group, language:Language) = {
    println("es " + psi + group + language);
    //println(marginalFor(group,parentOf(language)).fsa + " marg ");
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
  lazy val expectedMarginalScore: Seq[Map[Language,Double]] = {
    for ( groupMarginals <- marginals) 
    yield Map.empty ++ { for( language <- languages)
      yield { 
        val parent = parentOf(language);
        println(language);
        val marg = groupMarginals(parent);
        val parentMarginal = edgeFor(parent,language).fst.inputProjection;
        println("Start exp");
        val marginalCost = Transducer.logSpaceExpectationCompose(marg.fsa,parentMarginal).cost.value;
        println("end exp");
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
      new Marginal(this.fsa&m.fsa relabel);
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
      new Marginal(trace((fst >> c.fsa).inputProjection));
    }
    def parentMarginalize(p: Marginal) = {
      new Marginal(trace((p.fsa >> fst).outputProjection));
    }
  }

    }
  }

}
