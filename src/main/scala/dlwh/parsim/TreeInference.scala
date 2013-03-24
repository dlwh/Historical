package dlwh.parsim

import dlwh.cognates._
import breeze.util._

/**
 * 
 * @author dlwh
 */

class TreeInference[T,F<:Factors[T]](val factors: F, val tree: Tree[T], val cognates: T=>Seq[Cognate]) {
  import factors._
  def parent(l: T) = parentMap(l)
  private val parentMap = buildParentMap(tree, None)

  private def  buildParentMap(tree: Tree[T], parent: Option[T]):Map[T,Option[T]] = {
    tree.children.map(buildParentMap(_,Some(tree.label))).foldLeft(Map(tree.label->parent))(_ ++ _)
  }

  val hasUpwardMessage = {
    def recHasMessage(t: Tree[T]):Set[T] = {
      import scala.collection.breakOut
      val children: Set[T] = t.children.flatMap(recHasMessage)(breakOut)
      if(children.nonEmpty || cognates(t.label).nonEmpty)  children + t.label
      else Set.empty
    }
    recHasMessage(tree)
  }

  val children = {
    def buildChildren(tree: Tree[T], parent: Option[T]):Map[T,Set[T]] = {
      val myContribution = (tree.label -> (tree.children.map(_.label).toSet))
      tree.children.map(buildChildren(_,Some(tree.label))).foldLeft(Map(myContribution))(_ ++ _)
    }
    buildChildren(tree,None)
  }

  val upwardMessages:collection.mutable.Map[T,Belief] = new collection.mutable.HashMap[T,Belief] {
    override def default(l: T) = {
      val beliefs:Seq[Belief] = upwardBeliefs(l)
      val edge = edgeFor(l)
      val message = beliefs.map(b => edge.edgeMarginal(uniformBelief,b).parentProjection).reduceLeft(_ * _)
      update(l,message)
      message
    }
  }

  val upwardBeliefs: collection.mutable.Map[T,Seq[Belief]] = new collection.mutable.HashMap[T,Seq[Belief]] {
    override def default(l: T) = {
      val cogs = cognates(l)
      val beliefs = if(cogs.isEmpty) {
        val messages = for(c <- children(l) if hasUpwardMessage(c)) yield upwardMessages(c)
        Seq(messages.reduceLeft(_ * _))
      } else {
        cogs.map(c => indicatorBelief(c.word))
      }
      update(l,beliefs)
      beliefs
    }
  }

  // message to the factor (p->l) from p.
  // indexed by l.
  val downwardMessages: collection.mutable.Map[T,Belief] = new collection.mutable.HashMap[T,Belief] {
    override def default(l: T) = {
      val p = parent(l)
      val messageToP = p.map(downwardMessages).getOrElse(rootMessage(l))
      val messages = for(c <- p.toIterator.flatMap(children) if hasUpwardMessage(c) && l != c) yield upwardMessages(c)
      val qp = messages.foldLeft(messageToP)(_ * _)
      qp
    }

  }

  lazy val beliefs = hasUpwardMessage.iterator.map { l =>
    if(l == tree.label) l -> Seq(rootMessage(l) * upwardBeliefs(l).head)
    else l -> edgeMarginals(l).map(_.childProjection)
  }.toMap

  val edgeMarginals:collection.mutable.Map[T,Seq[EdgeMarginal]] = new collection.mutable.HashMap[T,Seq[EdgeMarginal]] {
    override def default(l: T) = {
      val qp = downwardMessages(l)
      val edge = edgeFor(l)
      val message = upwardBeliefs(l).map(edge.edgeMarginal(qp,_))
      update(l,message)
      message
    }
  }

  def sufficientStatistics: Map[T,factors.SufficientStatistic] = hasUpwardMessage.map{t => t ->
    edgeMarginals(t).map(_.sufficientStatistic).reduce(_ + _)
  }.toMap

  lazy val likelihood  = {
    val up = upwardBeliefs(tree.label).head
    val down = rootMessage(tree.label)
    up * down partition
  }

  def edgeMarginal(child: T):Seq[factors.EdgeMarginal] = if(hasUpwardMessage(child)) edgeMarginals(child) else Seq.empty
}