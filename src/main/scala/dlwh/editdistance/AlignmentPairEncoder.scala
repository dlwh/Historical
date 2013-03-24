package dlwh.editdistance

import breeze.util.Index
import breeze.linalg.Vector
import breeze.linalg._

/**
 * 
 * @author dlwh
 */

class AlignmentPairEncoder(val charIndex: Index[Char]) {
  lazy val epsIndex = charIndex('\0')
  lazy val doubleEpsilon = encode(epsIndex,epsIndex)
  def encode(p: Int, c: Int):Int = p + (c * charIndex.size)
  def encode(p: Char, c: Char):Int = charIndex(p) + (charIndex(c) * charIndex.size)
  def decode(pc:Int): (Int, Int) = (pc%charIndex.size, pc/charIndex.size)
  def decodeChar(pc: Int) = {
    val (a,b) = decode(pc)
    (charIndex.get(a),charIndex.get(b))
  }
  def decode(vec: Vector[Double]):Counter2[Char,Char,Double] = {
    val r = Counter2[Char,Char,Double]()
    for( (ind,v) <- vec.activeIterator) {
      val (pI,cI) =  decode(ind)
      r(charIndex.get(pI),charIndex.get(cI)) = v
    }
    r
  }

  def size = charIndex.size * charIndex.size
}
