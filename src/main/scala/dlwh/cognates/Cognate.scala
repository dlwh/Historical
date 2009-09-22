package dlwh.cognates;
import scala.io._;

case class Cognate(word: String, trueLanguage: String);

object Romance {
  def readCognates(): Seq[Seq[Cognate]]= {
    val stream = this.getClass.getClassLoader().getResourceAsStream("romance_ipa");
    val src = Source.fromInputStream(stream)(Codec.UTF8).getLines().drop(1);
    val cognates = (for(line <- src) yield {
      for( (w,l) <- line.split(' ') zip Array("Spanish","Italian","Latin","Portuguese")) 
        yield Cognate(w,l);
    }).toSequence;

    stream.close();
    cognates.toSequence;
  }
}
