package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class CognateDetector[MyGrouper<:Grouper,
        MyAff<:AffinityScorer]
        (grouperFactory: Grouper.Factory[MyGrouper],
         affFactory: AffinityScorer.Factory[MyAff]) {
  final def iterations(cognates: IndexedSeq[Cognate]): Iterator[State] = {
    Iterator.iterate(initialState(cognates))(step _);
  }
  final def initialState(cognates: IndexedSeq[Cognate]):State = {
    State(cognates.map(CognateGroup(_)), grouperFactory.initialGrouper, affFactory.initialScorer)
  }
  final protected def nextState(oldState: State, newGroups: IndexedSeq[CognateGroup]):State = {
    State(newGroups, grouperFactory.nextGrouper(oldState.grouper), affFactory.nextScorer(oldState.affScorer, newGroups));
  }

  final def step(state: State):State = {
    val (groupA,groupB,untouched) = state.grouper.determineSplit(state.groups);
    val groupsToMerge = state.grouper.determineGroupsToMerge(state.affScorer, groupA, groupB);

    val newGroups = (
      Set() ++ groupA ++ groupB ++ untouched
      -- groupsToMerge.iterator.map(_._1) -- groupsToMerge.iterator.map(_._2)
      ++ { for( (a,b) <- groupsToMerge) yield a.merge(b)}
    ).toIndexedSeq;

    nextState(state, newGroups);
  }

  case class State(groups: IndexedSeq[CognateGroup],
                   grouper: MyGrouper,
                   affScorer: MyAff);
}


