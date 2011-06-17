package dlwh.newcognates

import scalanlp.util.Index
import scalala.tensor.Vector;
import scalala.tensor.counters.Counters._;

/**
 * 
 * @author dlwh
 */

class AlignmentPairEncoder(val charIndex: Index[Char]) {
  lazy val epsIndex = charIndex('\0');
  lazy val doubleEpsilon = encode(epsIndex,epsIndex);
  def encode(p: Int, c: Int):Int = p + (c * charIndex.size);
  def encode(p: Char, c: Char):Int = charIndex(p) + (charIndex(c) * charIndex.size);
  def decode(pc:Int): (Int, Int) = (pc%charIndex.size, pc/charIndex.size);
  def decode(vec: Vector):PairedDoubleCounter[Char,Char] = {
    val r = PairedDoubleCounter[Char,Char];
    for( (ind,v) <- vec.activeElements) {
      val (pI,cI) =  decode(ind);
      r(charIndex.get(pI),charIndex.get(cI)) = v;
    }
    r
  }

  def size = charIndex.size * charIndex.size;
}