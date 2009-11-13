package dlwh.cognates;

import scalanlp.counters.Counters.PairedDoubleCounter;
import scalanlp.counters.Counters.DoubleCounter;
import scalanlp.counters.LogCounters._;
import scalanlp.stats.sampling.Multinomial;
import scalanlp.fst._;
import scalanlp.util.Log._;
import collection.{mutable=>muta}
import scalanlp.math.Semiring.LogSpace._;

import Types._;

class Gibbs(numGroups: Int= 1000) {

  def chain(words: Seq[Cognate], tree: Tree) = {
    val alphabet = Set.empty ++ words.iterator.flatMap(_.word.iterator);

    Iterator.iterate(initialState(words,tree,alphabet)) { startState =>
      var ll = 0.0;
      val wordsState = words.foldLeft( (startState) ) { (assignedState,word) =>
        globalLog.log(INFO)(word);

        val state = assignedState.forgetAssignment(word);

        val pGgW = LogDoubleCounter[Group]();
        for(group <- 0 until numGroups) {
          val prior = state.prior(group);
          val likelihood = state.likelihood(word,group);
          globalLog.log(INFO)("  " + group + ":" + likelihood);
          pGgW(group) = prior + likelihood;
        }
        val group = Multinomial(pGgW).draw;
        globalLog.log(INFO)("Choice" + group);
        ll += pGgW(group);
        state.assign(word,group);
      }
      wordsState.copy(likelihood = ll);
    }
  }

  final case class State(groupAssignments: Map[Cognate,Group], likelihood: Double, insideOutsides: Seq[InsideOutside]) {
    def prior(g: Int) = 0.0; // XXX todo
    def likelihood(word: Cognate, group: Group):Double = {
      insideOutsides(group).likelihoodWith(word);
    }

    def forgetAssignment(word: Cognate) = {
      val group = groupAssignments(word);
      val io = insideOutsides(group);
      val newIO = io.remove(word);
      val newIOS = insideOutsides.updated(group,newIO);
      val newGroups = groupAssignments - word;
      this.copy(insideOutsides = newIOS, groupAssignments = newGroups);
    }

    def assign(word: Cognate, group: Group) = {
      val io = insideOutsides(group);
      val newIOS = insideOutsides.updated(group,io.include(word.language,word.word,0.0));
      val newGroups = groupAssignments + (word -> group);
      this.copy(insideOutsides = newIOS, groupAssignments = newGroups);
    }
  }

  private def initialState(words: Seq[Cognate], tree: Tree, alphabet: Set[Char]) = {
    val groupAssignments = new muta.HashMap[Cognate,Group]();
    val factors = new TransducerFactors(tree,alphabet);
    val ios = for(g <- Array.range(0,numGroups)) yield {
      val io = new InsideOutside(tree, factors,
                                Map.empty.withDefaultValue(Map.empty));
      val cog = words(g%words.length);
      groupAssignments(cog) = g;
      io.include(cog.language,cog.word,0.0);
    }
    new State(Map.empty ++ groupAssignments,Math.NEG_INF_DOUBLE,ios);
  }
}

object BasicGibbs { 
  def main(arg: Array[String]) {
    globalLog.level = INFO;
    val cognates = Cognates.basic();
    val tree = Tree.basic;
    val allCogs = cognates.flatten;

    val gibbs = new Gibbs(allCogs.length);
    for( state <- gibbs.chain(allCogs,tree)) {
      globalLog.log(INFO)("likelihood" + state.likelihood);
      globalLog.log(INFO)(state.groupAssignments);
    }
  }
}


object RomanceGibbs { 
  def main(arg: Array[String]) {
    globalLog.level = DEBUG;
    val cognates = Cognates.romance().take(200);
    val tree = Tree.romance;
    val allCogs = cognates.flatten;

    val gibbs = new Gibbs(allCogs.length);
    for( state <- gibbs.chain(allCogs,tree)) {
      globalLog.log(INFO)("likelihood" + state.likelihood);
      globalLog.log(INFO)(state.groupAssignments);
    }
  }
}


