package dlwh.historical;

import scalala.Scalala._;
import scalala.tensor._;
import fixed._;
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

  def gaussian(center: Vector, cov: Matrix) {
    require(center.size == 2);
    val chol = cholesky(zeros(2,2) + cov) value;

    val t = linspace(0,2*Math.Pi);
    val x = t.like;
    x := cos(t);

    val y = t.like;
    y := sin(t);

    for { i <- 0 until x.size  } {
      val v = Vector2(x(i),y(i));
      val newV = chol * 1.5 * v value;
      x(i) = newV(0);
      y(i) = newV(1);
    }
    x += center(0);
    y += center(1);
    plot(y,x);
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
