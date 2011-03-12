package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class DeathTreeScorer(tree: Tree, deathScore: (Language,Language)=>Double) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) = { (groupB: CognateGroup) =>
    val nodesWithDescendants = (groupA merge groupB).nodesWithObservedDescendants(tree);
    def recursiveDeathScore(tree: Tree): Double = tree match {
      case t: Child => 0.0;
      case Ancestor(label,children) =>
        assert(nodesWithDescendants(label));
        children.iterator.map { c =>
          if(!nodesWithDescendants(c.label)) deathScore(label,c.label);
          else math.log(1-math.exp(deathScore(label,c.label))) + recursiveDeathScore(c);
        }.reduceLeft(_ + _);
    }
    recursiveDeathScore(tree);
  }
}

object DeathTreeScorer {
  class Factory(tree: Tree, deathScore: (Language,Language)=>Double) extends AffinityScorer.Factory[DeathTreeScorer] {
    def nextScorer(curScorer: DeathTreeScorer, groups: IndexedSeq[CognateGroup]) = {
      // TODO: learn death probs
      curScorer;
    }

    def initialScorer = {
      new DeathTreeScorer(tree,deathScore);
    }
  }
}