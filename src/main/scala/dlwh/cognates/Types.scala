package dlwh.cognates;

import scalanlp.fst._;

object Types {
  // Type aliases for documentation:
  type Language = String;
  type Word = String;
  type Group = Int;
  // typically p(ancestor|word)
  type Psi = Automaton[Double,_,Char];

  def trace[T](x: T) = { println(x); x }
  def TODO = error("TODO");
}
