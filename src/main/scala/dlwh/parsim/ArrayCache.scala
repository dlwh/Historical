package dlwh.parsim

import java.util.Arrays

/**
 * 
 * @author dlwh
 */

class ArrayCache(xSize: Int, ySize: Int)(baseFn: (Int,Int)=>Double) extends ((Int,Int)=>Double) {

  val cache = Array.fill(xSize)(null:Array[Double])

  def apply(v1: Int, v2: Int) = {
    var arr = cache(v1);
    if (arr eq null) {
      arr = new Array[Double](ySize);
      Arrays.fill(arr,Double.NaN);
      cache(v1) = arr;
    }
    var result = arr(v2);
    if(result.isNaN) {
      result = baseFn(v1,v2);
      assert(!result.isNaN)
      arr(v2) = result;
    }
    result;
  }

}
