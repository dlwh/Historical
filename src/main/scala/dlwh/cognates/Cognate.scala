package dlwh.cognates;
import scala.io._;
import java.io._;

case class Cognate(word: String, language: String);

object Cognates {
  def romance(): Seq[Seq[Cognate]]= {
    readCognates("romance_ipa",Seq("Spanish","Italian","Latin","Portuguese"));
  }

  def basic(): Seq[Seq[Cognate]]= {
    readCognates("basic_ipa",Seq("Aish","Bish"));
  }

  def readCognates(file: String, languages: Seq[String])={
    val stream = this.getClass.getClassLoader().getResourceAsStream(file);
    val src = Source.fromInputStream(stream)(Codec.UTF8).getLines().filter(!_.startsWith("#"));
    val cognates = (for(line <- src) yield {
      for( ((w,c),l) <- line.split(' ').zipWithIndex.toSeq zip languages if w != "?")
        yield Cognate(w,l);
    }).toSeq;

    stream.close();
    cognates.toSeq;
  }
}

object PruneCognates {
  def main(args: Array[String]) {
    val percentage = args(1).toDouble;
    val src = Source.fromFile(new File(args(0)))(Codec.UTF8).getLines().filter(!_.startsWith("#"));
    for(line <- src) {
      val words = for( w <- line.split(' ') ) yield {
        if(Math.random < percentage) "?"
        else w;
      }
      println(words.mkString(" "));
    }

  }
}

