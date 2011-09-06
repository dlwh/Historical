package dlwh.parsim

import scalanlp.stats.distributions.{Poisson, Rand}
import scalala.library.Numerics.lgamma
import math._
import phylo.{Event, Tree, BorrowingEvent}


/**
 * 
 * @author dlwh
 */

class PPTree(ntaxa: Int, tau: Double= 10000., mass: Double = 5E-6, immortalPoints: IndexedSeq[Event[Int]])  {
  case class State(points: IndexedSeq[BorrowingEvent[Int]],
                   forest: IndexedSeq[Tree[Int]],
                   appliedEvents: IndexedSeq[Event[Int]],
                   likelihood: Double, prior: Double)

  private val gamma = mass * tau * ntaxa * (ntaxa - 1)

  def initialSample = {
    for {
      n <- new Poisson(ntaxa * ntaxa * tau * mass)
      rr = IndexedSeq.fill(n)(BorrowingEvent.randBorrowingEvent(ntaxa, tau))
      r <- Rand.promote(rr)
    } yield r.toIndexedSeq
  }

  def initialState(likelihood: Tree[Int]=>Double) = {
    val points = initialSample.get
    val (new_trees, new_appliedevents) = BorrowingEvent.treeFromBorrowingEvents(immortalPoints ++ points, ntaxa)
    val ll = new_trees.map(likelihood).sum
    val prior = points.length * log( gamma) * -gamma - (0 until points.size).map(i => log(i + 1)).sum

    State(points, new_trees, new_appliedevents, ll, prior)
  }

  def resample(state: State, likelihoodFunction: Tree[Int]=>Double, temperature: Double = 1.0) = {
    proposals.foldLeft(state) { (state,prop) =>
      import state._
      val new_points = prop(state.points).get
      val (new_trees, new_appliedevents) = BorrowingEvent.treeFromBorrowingEvents(immortalPoints ++ new_points, ntaxa)
      val ll_data_ratio = {
        if(new_appliedevents == appliedEvents) 0.0
        else new_trees.map(likelihoodFunction).sum - state.likelihood
      }

      val ll_points_ratio = {
        if(new_points.length == points.length) 0.0
        else if(new_points.length < points.length) log(new_points.length / gamma)
        else log( gamma / (new_points.length + 1))
      }

      val ll_ratio = ll_data_ratio + ll_points_ratio
      if(log(random) <= ll_ratio/temperature) {
        State(new_points, new_trees, new_appliedevents, likelihood + ll_data_ratio, prior + ll_points_ratio)
      } else {
        state
      }
    }

  }

  private def add(s: IndexedSeq[BorrowingEvent[Int]]) = for {
    p <- BorrowingEvent.randBorrowingEvent(ntaxa, tau)
  } yield s :+ p

  private def delete(s: IndexedSeq[BorrowingEvent[Int]]) = for {
    kill <- Rand.choose(s)
  } yield s.filterNot(kill ==)

  private def move(s: IndexedSeq[BorrowingEvent[Int]]) = for {
    p <- Rand.choose(s)
    t = p.time
    new_t <- Rand.gaussian(t,tau/3.0)
    really_new_t = if(new_t > tau) 2 * tau - new_t
                       else if (new_t < 0.0) -new_t
                       else new_t
    new_p = p.copy(time = new_t)
  } yield Set(s:_*) - p + new_p toIndexedSeq

  private val proposals = IndexedSeq(add _,delete _,move _)

}