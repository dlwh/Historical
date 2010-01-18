package dlwh.cognates;


import scalanlp.stats.sampling._;
import scalanlp.counters.LogCounters._;
import scalanlp.stats.sampling.Multinomial;
import scalanlp.fst._;
import scalanlp.util.Log._;
import collection.{mutable=>muta}
import scalanlp.math.Semiring.LogSpace._;

import Types._;

class Gibbs(numGroups: Int= 1000, smoothing: Double=0.5) {

  def chain(words: Seq[Cognate], tree: Tree) = {
    val alphabet = Set.empty ++ words.iterator.flatMap(_.word.iterator);

    Iterator.iterate(initialState(words,tree,alphabet)) { startState =>
      var ll = 0.0;
      val wordsState = words.foldLeft( (startState) ) { (assignedState,word) =>
        globalLog.log(INFO)(word);

        val state = assignedState.forgetAssignment(word);

        val pGgW = LogDoubleCounter[Group]();
        for(group <- state.validTablesForLanguage(word.language)) {
          val prior = state.prior(group);
          val likelihood = state.likelihood(word,group);
          globalLog.log(INFO)("  " + group + ":" + likelihood + " " + prior);
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

  final case class State private (
    tree: Tree,
    groupAssignments: Map[Cognate,Group],
    likelihood: Double,
    tables: Seq[InsideOutside[TransducerFactors]],
    factors: TransducerFactors,
    occupiedTables: Int,
    firstUnoccupiedTable:Int) {


    def this(tree: Tree, f: TransducerFactors) = this(
      tree=tree,
      groupAssignments = Map.empty,
      likelihood= Double.NegativeInfinity,
      tables= Seq.empty ++ Seq(emptyTable(tree,f)),
      factors=f,
      occupiedTables =0,
      firstUnoccupiedTable=0
    );

    def validTablesForLanguage(l: Language) = {
      val openTables = for {
        (io,g) <- tables.iterator.zipWithIndex
        // only include the first unoccupied tables, and those occupied
        if !io.isEmpty || g == firstUnoccupiedTable
        if isValidForLanguage(g,l)
      } yield {
        g
      }

      openTables
    }

    def isValidForLanguage(g: Group, l: Language) = {
      tables(g).numOccupants(l) == 0;
    }

    def prior(g: Int) = {
      val numerator = (tables(g).numOccupants + smoothing)
      val denom = (groupAssignments.size + occupiedTables * smoothing + 0.0001);
      Math.log(numerator / denom);
    }

    def likelihood(word: Cognate, group: Group):Double = {
      tables(group).likelihoodWith(word);
    }

    /**
     * Forget that the word belongs to any group, so that he can come
     * back to the restaurant and be reseated.
     */
    def forgetAssignment(word: Cognate) = {
      if(!groupAssignments.contains(word)) {
        this 
      } else {
        val group = groupAssignments(word);
        val io = tables(group);
        val newIO = io.remove(word);
        val newTables = tables.updated(group,newIO);
        val newGroups = groupAssignments - word;

        val newFirstUnoccupiedTable = if (newIO.isEmpty) {
          group min firstUnoccupiedTable
        } else {
          firstUnoccupiedTable
        }

        val newOccupiedTables = if (newIO.isEmpty) {
          occupiedTables - 1
        } else {
          occupiedTables
        }

        this.copy(
          tables = newTables,
          groupAssignments = newGroups,
          firstUnoccupiedTable = newFirstUnoccupiedTable,
          occupiedTables = newOccupiedTables
        );
      }
    }


    /**
     * Put the customer at a table
     */
    def assign(word: Cognate, group: Group) = {
      val io = tables(group);

      val wasEmpty = io.numOccupants == 0;

      var newTables = tables.updated(group,io.include(word.language,word.word,0.0));
      val newGroups = groupAssignments + (word -> group);

      assert(!wasEmpty || group == firstUnoccupiedTable);
      val newUnoccupiedTable = if(wasEmpty) {
        val index = newTables.indexWhere(_.numOccupants == 0,group);
        if(index == -1) {
          // I feel dirty XXX
          newTables = newTables ++  Seq(emptyTable(tree, factors))
          newTables.size - 1;
        } else {
          index
        }
      } else {
        firstUnoccupiedTable
      }

      val newOccupiedTables = if(wasEmpty) {
        occupiedTables + 1
      } else {
        occupiedTables
      }

      this.copy(
        tables = newTables,
        groupAssignments = newGroups,
        firstUnoccupiedTable = newUnoccupiedTable,
        occupiedTables = newOccupiedTables
      );
    }
  }

  private def emptyTable(tree : Tree, f: TransducerFactors) = {
    new InsideOutside(tree,f,Map.empty withDefaultValue Map.empty);
  }


  private def initialState(words: Seq[Cognate], tree: Tree, alphabet: Set[Char]) = {
    val groupAssignments = new muta.HashMap[Cognate,Group]();
    val factors = new TransducerFactors(tree,alphabet) with PosUniPruning;
    new State(tree,factors);
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