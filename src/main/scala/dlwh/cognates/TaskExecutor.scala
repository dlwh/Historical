/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package dlwh.cognates

import scala.actors.Futures._;

object TaskExecutor {
  def doTasks[A](tasks: Collection[()=>A]):Collection[A] = {
    tasks.toList.map(f => future( f() )) map ( _ apply () );
  }
}
