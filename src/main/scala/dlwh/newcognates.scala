package dlwh


import dlwh.newcognates.compression.{NormalizedTransitions, Compressor}
import scalanlp.fst._;

/**
 * 
 * @author dlwh
 */

package object newcognates  {
  type Language = String;
  type Word = String;
  type Group = Int;

  type Psi = Automaton[Double,_,Char];
  type MessageCompressor[T] = Compressor[T,Char] with NormalizedTransitions[T,Char];
  def memoryString = {
    val r = Runtime.getRuntime;
    val free = r.freeMemory / (1024 * 1024);
    val total = r.totalMemory / (1024 * 1024);
    ((total - free) + "M used; " + free  + "M free; " + total  + "M total");
  }

  def gc(trace: String) = {
    println("Pre gc " + trace + " " + memoryString);
    System.gc();
    println("Post gc " + trace + " " + memoryString);
  }
}

