package dlwh.historical;

import scalala.Scalala._;
import WALS.Language;

object Plot {
  def wals(langs: Seq[Language]) = {
    val lats = langs.map(_.coords._1).toArray;
    val longs = langs.map(_.coords._2).toArray;
    scatter(longs,lats,ones(longs.length),ones(longs.length));
  }
}
