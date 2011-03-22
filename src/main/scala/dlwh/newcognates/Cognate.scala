package dlwh.newcognates

import scala.io._;
import java.io._;

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

case class CognateGroup private(cognates: Map[Language,Cognate]) {

  def gloss = cognates.values.head.gloss;

  def +(c: Cognate) = new CognateGroup(cognates + (c.language -> c))
  def -(c: Language) = new CognateGroup(cognates - c);

  override def toString() = {
    cognates.values.mkString("CognateGroup(",",",")");
  }
  def merge(b: CognateGroup) = new CognateGroup(cognates ++ b.cognates);

  def glosses = cognates.valuesIterator.map(_.gloss).toSet;

  def canMerge(b: CognateGroup) = !cognates.keys.exists(b.cognates.keySet);

  def prettyString(t: Tree) = {
    val nodes = nodesWithObservedDescendants(t);
    t.prettyString { label =>
      cognates.get(label).map(label + ": " + _) orElse Some(label).filter(nodes);
    }

  }

  def nodesWithObservedDescendants(t: Tree):Set[Language] = {
    if(cognates.contains(t.label)) {
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
  def apply(cogs: Cognate*):CognateGroup = new CognateGroup(Map.empty ++ cogs.map(c => c.language -> c));

  def empty = apply();
}