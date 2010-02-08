/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package dlwh.cognates

import scala.actors.Future;
import scala.actors.Futures._;

object TaskExecutor {
  def doTasks[A](tasks: Iterable[()=>A]):Iterable[A] = {
    tasks.toList.map(f => future( f() )) map ( _ apply () );
  }

  def asFutures[A](tasks: Iterable[()=>A]): Iterable[Future[A]] = {
    tasks.toList.map (f => future( f()))
  }


}
