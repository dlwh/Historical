package dlwh.cognates

import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.charset.Charset

object PruneDataset {
  def main(args: Array[String]) {
    val dataset_name = args(0);
    val languages = args(1).split(",");
    val death_prob = args(2).toDouble;
    val dataset = new Dataset(dataset_name,languages);
    val tree = dataset.tree;
    val prunedCognates = for(group <- dataset.cognates) yield prune(group,death_prob,tree);
    val out = new PrintWriter(new OutputStreamWriter(System.out,Charset.forName("UTF8")));
    for(group <- prunedCognates) {
      out.println(group.map(_.word).mkString("\t"));
    }
    out.close();
  }

  def prune(cognates: Seq[Cognate], deathProb: Double, tree: Tree) = {
    val survivingLanguages = Iterator.continually(simulateDeath(deathProb,tree)).find(s => !s.isEmpty && s != Set("dummy")).get;
    cognates.map { c =>
      if(survivingLanguages.contains(c.language)) c
      else c.copy(word = "?");
    }
  }

  def simulateDeath(deathProb: Double, tree: Tree):Set[String] = {
    if(math.random < deathProb) Set.empty
    else tree match {
      case t: Child => t.leaves;
      case a: Ancestor => a.children.map(simulateDeath(deathProb, _)).reduceLeft(_ ++ _);
    }
  }
}
