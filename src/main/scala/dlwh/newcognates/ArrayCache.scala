package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class ArrayCache(xSize: Int, ySize: Int)(baseFn: (Int,Int)=>Double) extends ((Int,Int)=>Double) {

  val cache = Array.fill(xSize,ySize){Double.NaN}

  def apply(v1: Int, v2: Int) = {
    var result = cache(v1)(v2)
    if(result.isNaN) {
      result = baseFn(v1,v2);
      assert(!result.isNaN)
      cache(v1)(v2) = result;
    }
    result;
  }

  def density = cache.foldLeft(0)(_ + _.count(!_.isNaN)) * 1.0 / (xSize * ySize);
}

