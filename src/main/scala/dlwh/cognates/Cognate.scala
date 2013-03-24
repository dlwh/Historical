package dlwh.cognates

import scala.io._
import java.io._
import breeze.config.{ArgumentParser, Configuration}
import breeze.linalg.Counter

case class Cognate(word: String, language: String, gloss: Symbol = 'None)

object Cognates {

  def readCognates(stream: InputStream, languages: Seq[String]=Seq.empty, hasGloss:Boolean = false) ={
    val src = Source.fromInputStream(stream)(Codec.UTF8).getLines().toIndexedSeq
    val ls = if(languages.length == 0) {
      inferLangs(src.head, hasGloss)
    } else {
      languages
    }
    val cognates =  if(hasGloss) {
      for(line <- src.filterNot(line => line.startsWith("#") || line.isEmpty)) yield {
        val Array(glossIndex:String, words @ _*) = line.split("\\s")
        val gloss = glossIndex.takeWhile(_ != '(') // drop any unique identifier
        for( (w,l) <- words zip ls if w != "?")
        yield Cognate(w,l,Symbol(gloss))
      }
    } else {
      for(line <- src.filterNot(line => line.startsWith("#") || line.isEmpty)) yield {
        for( (w,l) <- line.split(' ').toSeq zip ls if w != "?")
        yield Cognate(w,l)
      }
    }
    val ret = cognates.toIndexedSeq

    stream.close()
    ret
  }

  private def inferLangs(line: String, hasGloss: Boolean):IndexedSeq[String] = {
    val ret = line.split(" ").drop(1)
    ret
  }
}

/*
object PruneCognates {
  def main(args: Array[String]) {
    val percentage = args(1).toDouble
    val src = Source.fromFile(new File(args(0)))(Codec.UTF8).getLines().filter(!_.startsWith("#"))
    for(line <- src) {
      val words = for( w <- line.split(' ') ) yield {
        if(math.random < percentage) "?"
        else w
      }
      println(words.mkString(" "))
    }

  }
}
*/

case class CognateGroup private(cognates: Set[Cognate]) {
  lazy val cognatesByLanguage = cognates.iterator.map(c => c.language -> c).toMap

  def gloss = cognates.head.gloss

  def +(c: Cognate) = new CognateGroup(cognates + c)
  def -(c: Language) = new CognateGroup(cognates.filterNot(_.language == c))

  override def toString() = {
    cognates.mkString("CognateGroup(",",",")")
  }
  def merge(b: CognateGroup) = new CognateGroup(cognates ++ b.cognates)

  def glosses = cognates.map(_.gloss).toSet

  def canMerge(b: CognateGroup) = !cognatesByLanguage.keys.exists(b.cognatesByLanguage.keySet)

  def prettyString(t: Tree[String]) = {
    val nodes = nodesWithObservedDescendants(t)
    t.prettyString { label =>
      cognatesByLanguage.get(label).map(label + ": " + _) orElse Some(label).filter(nodes)
    }

  }

  def nodesWithObservedDescendants(t: Tree[String]):Set[Language] = {
    if(cognatesByLanguage.contains(t.label)) {
      Set(t.label)
    } else {
      val children = t.children.map(nodesWithObservedDescendants _).reduceLeft(_ ++ _)
      if(children.isEmpty) children
      else children + t.label
    }
  }
}

object CognateGroup {
  def apply(cogs: Cognate*):CognateGroup = new CognateGroup(cogs.toSet)

  def empty = apply()

  def writeCognateGroups(cogs: Iterable[CognateGroup], langs: Seq[Language], path: File, evalLanguage:Language="", evalWord: Map[Cognate,Cognate]=Map.empty) {
    val out = new PrintStream(new BufferedOutputStream(new FileOutputStream(path)),false, "UTF-8")
    out.println(langs.mkString("# ","\t",""))
    for(group <- cogs) {
      val cognates = group.cognatesByLanguage.values.toIndexedSeq
      val goldWords = cognates.flatMap(evalWord.get)
      val gold = if(goldWords.isEmpty) None else Some(Counter.count(goldWords:_*).argmax)
      val words = langs.map{l =>
        if(l == evalLanguage && !gold.isEmpty) gold.get.word
        else group.cognatesByLanguage.get(l).map(_.word).getOrElse("?")
      }
      out.println(words.mkString("","\t",""))
    }
    out.close()
  }
}

/*
object GoldWords {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)))
    val dataset = Dataset.fromConfiguration(config)
    ArgumentParser.stringParser
    val otherCognates = new FileDataset(new File(args(0)), config.readIn[String]("dataset.name"), dataset.languages)
    val base = dataset.base
    val leaves = dataset.tree.leaves + dataset.tree.label
    val myWords = base.cognates.map(_.filter(c => leaves(c.language) )).filterNot(_.isEmpty)
    writeGoldWords(myWords.map(g => CognateGroup(g:_*)),  new File("gold_map"), dataset.tree.label)
  }

  def writeGoldWords(cogs: Iterable[CognateGroup], path: File, evalLanguage: String) {
    val out = new PrintStream(new BufferedOutputStream(new FileOutputStream(path)),false, "UTF-8")
    out.println("Language\tWord\tGoldWord")
    for(group <- cogs gold <- group.cognatesByLanguage.get(evalLanguage) c <- group.cognates if c.language != evalLanguage) {
      out.println(c.language +"\t" + c.word + "\t" + gold.word)
    }
    out.close()
  }
}

object PrintClade {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)))
    val dataset = Dataset.fromConfiguration(config)
    val base = dataset.base
    val leaves = dataset.tree.leaves + dataset.tree.label
    val myWords = base.cognates.map(_.filter(c => leaves(c.language))).filterNot(_.isEmpty)
    CognateGroup.writeCognateGroups(myWords.map(g => CognateGroup(g:_*)), base.languages, new File("clade"))
  }
}
*/
