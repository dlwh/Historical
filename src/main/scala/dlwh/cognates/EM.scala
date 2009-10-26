package dlwh.cognates;

import scalanlp.counters.Counters.PairedDoubleCounter;
import scalanlp.counters.Counters.DoubleCounter;
import scalanlp.counters.LogCounters._;
import scalanlp.fst._;
import collection.{mutable=>muta}
import scalanlp.math.Semiring.LogSpace._;

import Types._;
import Factors._;

class EM(numGroups: Int= 1000) {

  def estimate(words: Seq[Cognate], tree: Tree) = {
    val alphabet = Set.empty ++ words.iterator.flatMap(_.word.iterator);
    Iterator.iterate(initialState(words,tree,alphabet)) { state =>
      // E-step: Calculate p(word|group) for each word and group
      // maintain expected counts to get a meta automaton.
      val groupAutoCounts = new muta.HashMap[Language,PairedDoubleCounter[Group,Automaton[Double,_,Char]]] {
        override def default(s: Language) = getOrElseUpdate(s,PairedDoubleCounter[Group,Psi]());
      };
      var likelihood = 0.0;
      for(c@Cognate(word,language) <- words) {
        println(word);
        // precompute parent automaton for each word.
        val edgeFactor = state.edgePotentialForLanguage(language)
        val auto = new Marginal(Automaton.constant(word,0.0));
        val psi : Psi= edgeFactor.childMarginalize(auto).fsa;
        println(c + "....");
        println(psi);

        val pGgW = LogDoubleCounter[Group]();
        for(group <- 0 until numGroups) {
          println("  " + group);
          val prior = state.prior(group);
          val likelihood = state.likelihood(psi,language,group);
          pGgW(group) = prior + likelihood;
        }
        likelihood += pGgW.logTotal;
        val normalizedCtr = normalize(pGgW);
        groupAutoCounts(language).transpose.getRow(psi) += normalizedCtr;
      }

      // sorta m-step, sorta e-step: calculate marginal probabilities of each interior
      // node in the tree
      val groupTrees = (for( group <- Seq.range(0,numGroups)) yield {
        insideOutside(group,groupAutoCounts,state,tree);
      });
      
      // m-step: recalculate edge potentials, marginalize out p(word|language,group)
      val edgePotentials = calculateEdgePotentials(state,groupTrees,tree,alphabet);

      new State(edgePotentials, likelihood);
    }
  }

  final class State(val factors: Factors, val likelihood: Double) {
    def prior(g: Int) = 0.0; // XXX todo
    def likelihood(psi: Psi, language: Language, group: Group):Double = {
      factors.expectedScore(psi,group,language);
    }
    def edgePotentialForLanguage(l: Language) = factors.edgeFor(factors.parentOf(l),l);
  }

  private def insideOutside(group: Group,
                groupStringCounts: muta.Map[Language,PairedDoubleCounter[Group,Psi]],
                state: State, tree: Tree) = {
    val psis = groupStringCounts.mapValues { pairCtr => 
      val counts = pairCtr(group);
      val psi = counts.foldLeft(Automaton.constant("",Math.NEG_INF_DOUBLE)) { (acc,psiCount) =>
        val (psi,count) = psiCount;
        (acc | psi.scaleInitialWeights(count).inputProjection).relabel
      }
      new Factors.Marginal(psi.inputProjection);
    }
    new InsideOutside(tree,state.factors.edgeFor _, psis );
  }

  private def calculateEdgePotentials(state: State, groupTrees: Seq[InsideOutside], tree: Tree, alphabet: Set[Char]): Factors = {
    val newFactors = for(io <- groupTrees) yield { (m: Language)=> io.marginalFor(m) }
    new Factors(tree,newFactors,groupTrees.length,alphabet)
  }

  private def initialState(words: Seq[Cognate], tree: Tree, alphabet: Set[Char]) = {
    val langWords = words.groupBy(_.language);
    val editDistance = new Factors.SimpleEdgeTransducer(-1,-1,alphabet);
    val marginals = for(g <- Array.range(0,numGroups)) yield {
      Map.empty ++ langWords.map { case (l,_) =>
        val auto = Automaton.constant(langWords(l)(g).word,0.0);
        (l,new Marginal(auto));
      }
    }
    val earlyFactors = new Factors(tree,marginals,numGroups,alphabet);
    val IOs = for(m <- marginals) yield {
      new InsideOutside(tree,earlyFactors.edgeFor _ , m);
    }
    val factors = calculateEdgePotentials(null, IOs, tree, alphabet);
    new State(factors, Math.NEG_INF_DOUBLE);
  }
}

object RomanceEM { 
  def main(arg: Array[String]) {
    val cognates = Cognates.romance().take(10);
    val tree = Tree.romance;
    val allCogs = cognates.flatten;

    val em = new EM(20);
    for( states <- em.estimate(allCogs,tree)) {
      println(states.likelihood);
    }
  }
}

object BasicEM { 
  def main(arg: Array[String]) {
    val cognates = Cognates.basic();
    val tree = Tree.basic;
    val allCogs = cognates.flatten;

    val em = new EM(2);
    for( states <- em.estimate(allCogs,tree)) {
      println(states.likelihood);
    }
  }
}
