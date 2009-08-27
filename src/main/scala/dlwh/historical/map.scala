package dlwh.historical;

import scalala.Scalala._;
import scalala.tensor._;
import WALS.Language;

object Plot {
  def setBounds() = {
    xlim(-90,90)
    ylim(-90,90)
  }
  def wals(langs: Seq[Language]) = {
    setBounds();
    val lats = langs.map(_.coords(0)).toArray;
    val longs = langs.map(_.coords(1)).toArray;
    scatter(longs,lats,ones(longs.length),ones(longs.length));
  }

  def circle(center: Vector, radius: Double) {
    require(center.size == 2);

    val t = linspace(0,2*Math.Pi);
    val x = t.like;
    x := cos(t);
    x *= radius;
    x += center(1);

    val y = t.like;
    y := sin(t);
    y *= radius;
    y += center(0);
    plot(x,y);
  }
}
