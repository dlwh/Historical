package dlwh.cognates

import breeze.config.Configuration
import breeze.util.Index

/**
 * 
 * @author dlwh
 */

trait Dataset  {
  def tree: Tree[String]
  def languages: Seq[Language]
  def cognates: IndexedSeq[Seq[Cognate]]
  def base:Dataset

  lazy val alphabet: Index[Char] = {
    val allChars = cognates.iterator.flatMap(_.iterator).flatMap(_.word.iterator)
    Index(allChars.toSet + '\0')
  }
}

object Dataset {

  def simple(tree: Tree[String], languages: Seq[Language], cognates: IndexedSeq[Seq[Cognate]], full: Option[Dataset]=None):Dataset = {
    val t = tree
    val l = languages
    val c = cognates

    new Dataset {
      val tree = t
      val languages = l
      val cognates = c
      val base:Dataset = full getOrElse this
    }
  }

  def fromConfiguration(config: Configuration, filterLeaves: Boolean = false):Dataset = {
    val languages = config.readIn[String]("dataset.languages").split(",")
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false)

    val dataset_name = config.readIn[String]("dataset.name")
    val dataset = new ResourceDataset(dataset_name,languages, withGloss)
    val basetree: Tree[String] = dataset.tree
    val tree = basetree.subtreeAt(config.readIn[String]("subtree",basetree.label))
    val leaves = tree.leaves.toSet
    val cogs = if(filterLeaves) dataset.cognates.map(_.filter(cog => leaves(cog.language))).filterNot(_.isEmpty)
               else dataset.cognates.filter(_.nonEmpty)
    simple(tree,languages,cogs,Some(dataset))
  }
}
