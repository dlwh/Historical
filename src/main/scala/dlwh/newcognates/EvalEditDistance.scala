package dlwh.newcognates

import java.io.File
import io.{Codec, UTF8Codec, Source}
import collection.immutable.Map

/**
 * 
 * @author dlwh
 */

object EvalEditDistance {
  def editDistance(s: String, t: String) = {
    // for all i and j, d[i,j] will hold the Levenshtein distance between
    // the first i characters of s and the first j characters of t;
    // note that d has (m+1)x(n+1) values
    val m = s.length
    val n = t.length
    val d = Array.ofDim[Int](m+1,n+1)
    for (i <- 0 to m)
      d(i)(0) = i
    for (j <- 0 to n)
      d(0)(j) = j // the distance of any second string to an empty first string

    for(j <- 1 to n; i <- 1 to m) {
      if (s(i-1) == t(j-1))
        d(i)(j) = d(i-1)(j-1)       // no operation required
      else d(i)(j) = (  (d(i-1)(j) + 1) min  // a deletion
        (d(i)(j-1) + 1) min  // an insertion
        (d(i-1)(j-1) + 1) // a substitution
      )
    }

    d(m)(n)
  }

  def readCognates(f: File): Map[Cognate, String] = {
    val entries = for( l <- Source.fromFile(f)(Codec.UTF8).getLines()) yield {
      val Array(language,word,gold,x@_*) = l.trim.split("\\s+");
      Cognate(word,language) -> gold
    }
    entries.toMap;
  }

  def main(args: Array[String]) {
    val gold = readCognates(new File(args(0)));
    val guess = readCognates(new File(args(1)));

    val eds = for( (cog,guessed) <- guess if gold contains cog) yield {
      val ed = editDistance(guessed,gold(cog));
      println(cog,guessed,gold(cog),ed);
      ed
    }
    import scalala.tensor.counters.Counters._;
    val countOfCounts = count(eds);
    println(countOfCounts);
    println(scalanlp.stats.DescriptiveStats.variance(eds.iterator.map(_.toDouble)));
    println("Size: " + eds.size);
    println(":: Total: " + eds.sum * 1.0 /eds.size);
  }
}