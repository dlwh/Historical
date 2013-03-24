package dlwh.util

/**
 * 
 * @author dlwh
 */
trait Lazy[+T] {
  def result: T

}

object Lazy {
  def delay[T](thunk: =>T) = {
    val xx = () => thunk
    new Lazy[T] {
      private var result_ : T = _
      def result = {
        if(_thunk ne null) {
          result_ = _thunk()
          _thunk = null
        }
        result_
      }

      private var _thunk: ()=>T = xx

    }
  }
}
