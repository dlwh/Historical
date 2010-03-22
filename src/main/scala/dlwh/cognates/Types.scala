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
  def memoryString = {
    val r = Runtime.getRuntime;
    val free = r.freeMemory / (1024 * 1024);
    val total = r.totalMemory / (1024 * 1024);
    ((total - free) + "M used; " + free  + "M free; " + total  + "M total");
  }

  type MessageCompressor[T] = Compressor[T,Char] with dlwh.cognates.NormalizedTransitions[T,Char];
}
