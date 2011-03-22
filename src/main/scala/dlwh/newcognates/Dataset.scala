package dlwh.newcognates

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

  lazy val alphabet: Index[Char] = {
     Index(cognates.iterator.flatMap(_.iterator).flatMap(_.word.iterator) ++ Iterator.single(implicitly[Alphabet[Char]].epsilon));
  }
}

object Dataset {

  def simple(tree: Tree, languages: Seq[Language], cognates: IndexedSeq[Seq[Cognate]]):Dataset = {
    val t = tree;
    val l = languages;
    val c = cognates;

    new Dataset {
      val tree = t;
      val languages = l;
      val cognates = c;
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
    simple(tree,languages,cogs);
  }
}