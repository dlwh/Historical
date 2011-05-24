package dlwh.newcognates

import scala.io._;
import java.io._
import scalanlp.config.Configuration
;

case class Cognate(word: String, language: String, gloss: Symbol = 'None);

object Cognates {
  def romance(): Seq[Seq[Cognate]]= {
    readCognates("romance_ipa",Seq("Spanish","Italian","Latin","Portuguese"));
  }

  def basic(): Seq[Seq[Cognate]]= {
    readCognates("basic_ipa",Seq("Aish","Bish"));
  }

  def readCognates(file: String, languages: Seq[String]=Seq.empty, hasGloss:Boolean = false) ={
    val ls = if(languages.length == 0) {
      inferLangs(file, hasGloss)
    } else {
      languages
    };
    val stream = this.getClass.getClassLoader().getResourceAsStream(file);
    val src = Source.fromInputStream(stream)(Codec.UTF8).getLines().filter(!_.startsWith("#"));
    val cognates =  if(hasGloss) {
      for(line <- src) yield {
        val Array(glossIndex:String, words @ _*) = line.split("\\s");
        val gloss = glossIndex.takeWhile(_ != '('); // drop any unique identifier
        for( (w,l) <- words zip ls if w != "?")
        yield Cognate(w,l,Symbol(gloss));
      }
    } else {
      for(line <- src) yield {
        for( (w,l) <- line.split(' ').toSeq zip ls if w != "?")
        yield Cognate(w,l);
      }
    }
    val ret = cognates.toIndexedSeq;

    stream.close();
    ret
  }

  private def inferLangs(file: String, hasGloss: Boolean):IndexedSeq[String] = {
    val stream = this.getClass.getClassLoader().getResourceAsStream(file);
    val line = Source.fromInputStream(stream)(Codec.UTF8).getLines().next;

    
    val ret = line.split(" ").drop(1);
    stream.close();
    ret;
  }
}

object PruneCognates {
  def main(args: Array[String]) {
    val percentage = args(1).toDouble;
    val src = Source.fromFile(new File(args(0)))(Codec.UTF8).getLines().filter(!_.startsWith("#"));
    for(line <- src) {
      val words = for( w <- line.split(' ') ) yield {
        if(math.random < percentage) "?"
        else w;
      }
      println(words.mkString(" "));
    }

  }
}

case class CognateGroup private(cognates: Set[Cognate]) {
  lazy val cognatesByLanguage = cognates.iterator.map(c => c.language -> c).toMap

  def gloss = cognates.head.gloss;

  def +(c: Cognate) = new CognateGroup(cognates + c)
  def -(c: Language) = new CognateGroup(cognates.filterNot(_.language == c));

  override def toString() = {
    cognates.mkString("CognateGroup(",",",")");
  }
  def merge(b: CognateGroup) = new CognateGroup(cognates ++ b.cognates);

  def glosses = cognates.map(_.gloss).toSet;

  def canMerge(b: CognateGroup) = !cognatesByLanguage.keys.exists(b.cognatesByLanguage.keySet);

  def prettyString(t: Tree) = {
    val nodes = nodesWithObservedDescendants(t);
    t.prettyString { label =>
      cognatesByLanguage.get(label).map(label + ": " + _) orElse Some(label).filter(nodes);
    }

  }

  def nodesWithObservedDescendants(t: Tree):Set[Language] = {
    if(cognatesByLanguage.contains(t.label)) {
      Set(t.label)
    } else t match {
      case t: Child => Set.empty;
      case a: Ancestor =>
        val children = a.children.map(nodesWithObservedDescendants _).reduceLeft(_ ++ _);
        if(children.isEmpty) children;
        else children + t.label;
    }
  }
}

object CognateGroup {
  def apply(cogs: Cognate*):CognateGroup = new CognateGroup(cogs.toSet);

  def empty = apply();

  def writeCognateGroups(cogs: Iterable[CognateGroup], langs: Seq[Language], path: File, evalLanguage:Language="", evalWord: Map[Cognate,Cognate]=Map.empty) {
    val out = new PrintStream(new BufferedOutputStream(new FileOutputStream(path)),false, "UTF-8");
    out.println(langs.mkString("# ","\t",""));
    for(group <- cogs) {
      val cognates = group.cognatesByLanguage.values.toIndexedSeq;
      val goldWords = cognates.flatMap(evalWord.get);
      val gold = if(goldWords.isEmpty) None else Some(scalala.tensor.counters.Counters.count(goldWords).argmax);
      val words = langs.map{l =>
        if(l == evalLanguage && !gold.isEmpty) gold.get.word
        else group.cognatesByLanguage.get(l).map(_.word).getOrElse("?")
      }
      out.println(words.mkString("","\t",""));
    }
    out.close();
  }
}


object PrintClade {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val dataset = Dataset.fromConfiguration(config);
    val base = dataset.base;
    val leaves = dataset.tree.leaves + dataset.tree.label;
    val myWords = base.cognates.map(_.filter(c => leaves(c.language))).filterNot(_.isEmpty);
    CognateGroup.writeCognateGroups(myWords.map(g => CognateGroup(g:_*)), base.languages, new File("clade"));
  }
}