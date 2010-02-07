/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package dlwh.cognates

import scala.actors.Futures._;

object TaskExecutor {
  def doTasks[A](tasks: Iterable[()=>A]):Iterable[A] = {
    tasks.toList.map(f => future( f() )) map ( _ apply () );
  }

  def iterateTasks[A](tasks: Iterator[() => A]):Iterator[A] = {
    val listed = tasks.toList;
    var numLeft = tasks.length;
    val outputQueue = new collection.mutable.Queue[A]();
    listed foreach { f =>
      val fut = future {
        val a = f();
        outputQueue synchronized {
          outputQueue += a
          numLeft -= 1
        }
      }
    }

    new Iterator[A] {
      def next = outputQueue synchronized { outputQueue.dequeue }
      def hasNext = outputQueue synchronized { numLeft > 0 || !outputQueue.isEmpty }
    }

  }
}
