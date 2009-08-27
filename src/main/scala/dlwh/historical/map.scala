package dlwh.historical;

import scalala.Scalala._;
import WALS.Language;

object Plot {
  def wals(langs: Seq[Language]) = {
    val lats = langs.map(_.coords._1).toArray;
    val longs = langs.map(_.coords._2).toArray;
    scatter(longs,lats,ones(longs.length),ones(longs.length));
  }

  def circle(center: (Double,Double), radius: Double) {
    val t = linspace(0,2*Math.Pi);
    val x = t.like;
    x := cos(t);
    x *= radius;
    x += center._2;

    val y = t.like;
    y := sin(t);
    y *= radius;
    y += center._1;
    plot(x,y);
  }
}
