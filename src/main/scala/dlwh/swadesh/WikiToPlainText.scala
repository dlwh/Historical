package dlwh.swadesh

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.charset.Charset
import org.apache.commons.compress.compressors.bzip2._;
import info.bliki.wiki.filter.PlainTextConverter;
import info.bliki.wiki.model.WikiModel;

import scala.io.Source;
import scala.io.Codec;
import scala.xml.pull._;

object WikiToPlainText {
  def main(args: Array[String]) {
    val wikiModel = new WikiModel("http://www.mywiki.com/wiki/${image}", "http://www.mywiki.com/wiki/${title}");
    val in = new FileInputStream(args(0));
    val fileOut = (new BufferedOutputStream(new FileOutputStream(args(1))));
    val out = new PrintWriter(new OutputStreamWriter(new BZip2CompressorOutputStream(fileOut),Charset.forName("UTF-8")));
    val bzIn = new BZip2CompressorInputStream(new BufferedInputStream(in));
    val source = Source.fromInputStream(bzIn)(Codec.UTF8);
    val er = new XMLEventReader(source);

    val entities = Map(
      "quot" -> "\"",
      "amp"	-> "&",
      "apos" -> "'",
      "lt" -> "<",
      "gt" -> ">"
    );

    var nextIsText = false;
    var nextIsTitle = false;
    var articleName = "";
    var buf = new StringBuilder;

    while(er.hasNext) {
     val e:XMLEvent = er.next;

     e match {
       case EvElemStart(_,"title",_,_) =>
        nextIsTitle = true;
        nextIsText = false;
       case EvElemStart(_,"text",_,_) =>
        nextIsText = true;
        nextIsTitle = false;
       case EvElemEnd(_,"title") =>
         nextIsTitle = false;
         nextIsText = false;
       case EvElemEnd(_,"text") =>
         val plainStr = wikiModel.render(new PlainTextConverter(), buf.toString);
         val myXml = <article><title>{articleName}</title><text>{plainStr}</text><wiki>{buf}</wiki></article>;
         println(articleName);
         out.println(myXml);
         nextIsText = false;
         nextIsTitle = false;
         buf = new StringBuilder;
         articleName = ""
       case e: EvText if nextIsTitle =>
         articleName = articleName + e.text;
       case e: EvText if nextIsText =>
         buf ++= e.text;
       case EvEntityRef(lbl) if nextIsText =>
         buf ++= entities(lbl);
       case EvEntityRef(lbl) if nextIsTitle =>
         articleName += entities(lbl);
       case _ =>
     }
    }

    out.close();
    source.close();
  }
}
