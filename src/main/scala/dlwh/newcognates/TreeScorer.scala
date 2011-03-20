package dlwh.newcognates

/**
 * 
 * @author dlwh
 */
class TreeScorer[F<:Factors](tree: Tree, factors: F) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) = {
    {(groupB: CognateGroup) =>
      val bigGroup = groupA merge groupB;
      val infer = new TreeInference(factors,tree,bigGroup);
      val ll = infer.onePassBeliefs.downward.likelihood;
//      val ll = infer.onePassBeliefs.likelihood;
      ll
    }
  }
}

object TreeScorer {
  def factory[F<:Factors](tree: Tree, f: F):AffinityScorer.Factory[TreeScorer[F]] = new AffinityScorer.Factory[TreeScorer[F]] {
    def nextScorer(curScorer: TreeScorer[F], groups: IndexedSeq[CognateGroup]) = new TreeScorer(tree,f);

    def initialScorer = new TreeScorer[F](tree,f);
  }
}

/**
 *
 * @author dlwh
 */
class TreeEliminationScorer[F<:Factors](tree: Tree, factors: F) extends AffinityScorer {
  def calibrate(groupA: CognateGroup) = {
    {(groupB: CognateGroup) =>
      val bigGroup = groupA merge groupB;
      val infer = new TreeElimination(factors,tree,bigGroup);
      val ll = infer.likelihood;
//      println(groupA + "X" + groupB + " " + ll);
      ll
    }
  }
}

object TreeEliminationScorer {
  def factory[F<:Factors](tree: Tree, f: F):AffinityScorer.Factory[TreeEliminationScorer[F]] = new AffinityScorer.Factory[TreeEliminationScorer[F]] {
    def nextScorer(curScorer: TreeEliminationScorer[F], groups: IndexedSeq[CognateGroup]) = curScorer;

    def initialScorer = new TreeEliminationScorer[F](tree,f);
  }
}

