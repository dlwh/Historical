package dlwh.baldur

import scalanlp.config.Configuration
import scalanlp.util.Index
import scalanlp.fst.Alphabet

/**
 * 
 * @author dlwh
 */

trait Dataset  {
  def tree: Tree
  def languages: Seq[Language]
  def cognates: IndexedSeq[Seq[Cognate]];
  def base:Dataset;

  lazy val alphabet: Index[Char] = {
    val allChars = cognates.iterator.flatMap(_.iterator).flatMap(_.word.iterator);
    Index(allChars.toSet + '\0');
  }
}

object Dataset {

  def simple(tree: Tree, languages: Seq[Language], cognates: IndexedSeq[Seq[Cognate]], full: Option[Dataset]=None):Dataset = {
    val t = tree;
    val l = languages;
    val c = cognates;

    new Dataset {
      val tree = t;
      val languages = l;
      val cognates = c;
      val base = full getOrElse this;
    }
  }

  def fromConfiguration(config: Configuration):Dataset = {
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new ResourceDataset(dataset_name,languages, withGloss);
    val basetree: Tree = dataset.tree;
    val tree = basetree.subtreeAt(config.readIn[String]("subtree",basetree.label));
    val leaves = tree.leaves;
    val cogs = dataset.cognates.map(_.filter(cog => leaves(cog.language))).filterNot(_.isEmpty);;
    simple(tree,languages,cogs,Some(dataset));
  }
}
