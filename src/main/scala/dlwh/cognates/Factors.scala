package dlwh.cognates;

import scalanlp.math.Semiring;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.fst._;
import scalanlp.util.Log._;

import Factors._;
import Types._;

class Factors(t: Tree, marginals: Seq[Language=>Marginal], numGroups: Int, alphabet: Set[Char]) {
  def edgeFor(parent: String, child: String, alphabet: Set[Char]): EdgeFactor = ef;
  val ef = new EdgeFactor(new EditDistance(-1,-1,alphabet));
  def parentOf(l: String): String = parents(l);
  def expectedScore(psi: Psi, group: Group, language:Language) = {
    //globalLog.log(DEBUG)(marginalFor(group,parentOf(language)).fsa + " marg ");
    val expectedJointScore = ExpectationComposition.logSpaceExpectationCompose(marginalFor(group,parentOf(language)).fsa,psi).cost.value;
    assert(!expectedJointScore.isNaN);
    val margScore = expectedMarginalScore(group)(language);
    assert(!margScore.isNaN);
    globalLog.log(DEBUG)("scores" + margScore + " " +expectedJointScore);
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
        globalLog.log(DEBUG)(language);
        val marg = groupMarginals(parent);
        val parentMarginal = edgeFor(parent,language,alphabet).fst.inputProjection;
        globalLog.log(DEBUG)("Start exp");
        val marginalCost = ExpectationComposition.logSpaceExpectationCompose(marg.fsa,parentMarginal).cost.value;
        globalLog.log(DEBUG)("end exp");
        (language,marginalCost);
      }
    }
  }
}

object Factors {
  class Marginal(val fsa: Psi) {
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
  class EdgeFactor(val fst: Transducer[Double,_,Char,Char]) {
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

  def simpleEdge(alphabet: Set[Char], fullBet: Set[Char]) = {
    new EdgeFactor(new EditDistance(-3,-4,alphabet,rhoSize=fullBet.size-alphabet.size));
  }
  def decayMarginal(alphabet: Set[Char], fullBet: Set[Char]) ={
    new Marginal(new DecayAutomaton(5.0,alphabet,rhoSize=fullBet.size-alphabet.size));
  }
}
