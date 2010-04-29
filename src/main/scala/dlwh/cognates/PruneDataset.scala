package dlwh.cognates

object PruneDataset {
  def main(args: Array[String]) {
    val dataset_name = args(0);
    val languages = args(1).split(",");
    val death_prob = args(2).toDouble;
    val dataset = new Dataset(dataset_name,languages);
    val tree = dataset.tree;
    val prunedCognates = for(cognate <- dataset.cognates) yield prune(cognate,death_prob,tree);
  }

  def prune(cognates: Seq[Cognate], deathProb: Double, tree: Tree) = {
    val survivingLanguages = simulateDeath(deathProb,tree)
    cognates.map {
      if(
    }
  }

  def simulateDeath(deathProb: Double, tree: Tree):Set[String] = {
    if(Math.random < deathProb) Set.empty
    else tree match {
      case t: Child => t.leaves;
      case a: Ancestor => a.children.map(simulateDeath(deathProb, _)).reduceLeft(_ ++ _);
    }
  }
}
