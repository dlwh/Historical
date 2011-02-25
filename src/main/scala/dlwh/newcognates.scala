package dlwh


import cognates.{NormalizedTransitions, Compressor}
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
}

