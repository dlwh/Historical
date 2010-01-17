package dlwh.cognates;
import scala.io._;

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
      for( ((w,c),l) <- line.split(' ').zipWithIndex.toSeq zip languages) 
        yield Cognate(w,l);
    }).toSeq;

    stream.close();
    cognates.toSeq;
  }
}

