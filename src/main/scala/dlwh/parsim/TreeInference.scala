package dlwh.parsim

import dlwh.cognates._
import phylo.Tree
import scalanlp.util._

/**
 * 
 * @author dlwh
 */

class TreeInference[T,F<:Factors[T]](val factors: F, val tree: Tree[T], val cognates: T=>Option[Cognate]) {
  import factors._;
  def parent(l: T) = parentMap(l);
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
      val myContribution = (tree.label -> (tree.children.map(_.label).toSet));
      tree.children.map(buildChildren(_,Some(tree.label))).foldLeft(Map(myContribution))(_ ++ _)
    }
    buildChildren(tree,None);
  }

  val upwardMessages:collection.mutable.Map[T,Belief] = new collection.mutable.HashMap[T,Belief] {
    override def default(l: T) = {
      val b:Belief = upwardBeliefs(l);
      val message = edgeFor(l).edgeMarginal(uniformBelief,b).parentProjection;
      update(l,message);
      message
    }
  }

  val upwardBeliefs: collection.mutable.Map[T,Belief] = new collection.mutable.HashMap[T,Belief] {
    override def default(l: T) = {
      val belief = cognates(l) match {
        case Some(cognate) =>
          indicatorBelief(cognate.word)
        case None =>
          val messages = for(c <- children(l) if hasUpwardMessage(c)) yield upwardMessages(c);
          messages.reduceLeft(_ * _);
      }
      update(l,belief);
      belief;
    }
  }

  // message to the factor (p->l) from p.
  // indexed by l.
  val downwardMessages: collection.mutable.Map[T,Belief] = new collection.mutable.HashMap[T,Belief] {
    override def default(l: T) = {
      val p = parent(l)
      val messageToP = p.map(downwardMessages).getOrElse(rootMessage)
      val messages = for(c <- p.toIterator.flatMap(children) if hasUpwardMessage(c) && l != c) yield upwardMessages(c)
      val qp = messages.foldLeft(messageToP)(_ * _);
      qp
    }

  }

  lazy val beliefs = hasUpwardMessage.iterator.map { l =>
    if(l == tree.label) l -> (rootMessage * upwardBeliefs(l))
    else l -> edgeMarginals(l).childProjection;
  } toMap

  val edgeMarginals:collection.mutable.Map[T,EdgeMarginal] = new collection.mutable.HashMap[T,EdgeMarginal] {
    override def default(l: T) = {
      val qp = downwardMessages(l);
      val message = edgeFor(l).edgeMarginal(qp,upwardBeliefs(l));
      update(l,message);
      message
    }
  }

  def sufficientStatistics: Map[T,factors.SufficientStatistic] = hasUpwardMessage.map(t => t -> edgeMarginals(t).sufficientStatistic).toMap

  lazy val likelihood  = {
    val up = upwardBeliefs(tree.label)
    val down = rootMessage;
    up * down partition;
  };

  def edgeMarginal(child: T) = if(hasUpwardMessage(child)) Some(edgeMarginals(child)) else None;
}