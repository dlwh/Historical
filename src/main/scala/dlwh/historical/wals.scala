package dlwh.historical;

import scala.io._;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.immutable.IntMap;

import scalala.tensor._;
import scalala.tensor.fixed._;

import scalanlp.counters.Counters._;

object WALS {

  case class Language(shortName: String, name: String, altLang: String, coords: Vector, 
    family: String, genus: String, features: Map[Int,Int], eths: Map[String,String]);

  lazy val all: Seq[Language] = {
    val stream = this.getClass.getClassLoader().getResourceAsStream("wals_new");
    // read until we get to a blank line.
    val lines = Source.fromInputStream(stream).getLines().dropWhile(_.trim != "").drop(1);

    val buf = new ArrayBuffer[Language];
    for (descripLine <- lines) {
      val featureLine = lines.next.trim;
      val Array(_, _shortName, _name, _altLang, "?", coordString, _family, _genus) = descripLine.trim.split("\t");
      val _coords = {
        val Array(lat,long) = coordString.split(" ").map(_.toDouble);
        Vector2(lat,long);
      }
      val _features = Map.empty ++ featureLine.split(" ").zipWithIndex.filter( _._1 !=  "?").map( x => (x._2,x._1.toInt) )
      
      var ethLines = lines.takeWhile(_.trim != "");
      val _eths = Map.empty ++ ( for(e <- ethLines) yield {
        val Array("eth",key,value) = e.trim.split("\t");
        (key,value);
      } );

      buf += Language ( shortName = _shortName, name = _name, 
        altLang = _altLang, coords = _coords,
        family = _family.intern, genus = _genus.intern,
        features = _features, eths = _eths
      )
    }

    stream.close();
    buf
  }

  lazy val daumeIE = {
    val langs = all.filter(_.family == "Indo-European").filter(_.name != "Afrikaans").filter(_.features.size > 10);
    val counts = count(langs.iterator.flatMap(_.features.keysIterator));
    val min = langs.size / 4;

    // result:
    for (lang <- langs) yield {
      val newFeatures = lang.features.filter { case (k,v)  => counts(k) >= min }
      lang.copy(features=newFeatures);
    }

  }

  lazy val daumeAll = {
    val langs = all.filter(_.features.size > 25);
    val counts = count(langs.iterator.flatMap(_.features.keysIterator));
    val min = langs.size / 10;

    // result:
    for (lang <- langs) yield {
      val newFeatures = lang.features.filter { case (k,v)  => counts(k) >= min }
      lang.copy(features=newFeatures);
    }

  }

}
