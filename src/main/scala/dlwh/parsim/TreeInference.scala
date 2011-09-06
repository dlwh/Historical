package dlwh.parsim

import dlwh.cognates._
import phylo.Tree
import scalanlp.util._

/**
 * 
 * @author dlwh
 */

class TreeInference[T,F<:Factors[T]](val factors: F, val tree: Tree[T], val group: CognateGroup) {
  import factors._;
  def parent(l: T) = parentMap(l);
  private val parentMap = buildParentMap(tree, None)

  private def  buildParentMap(tree: Tree[T], parent: Option[T]):Map[T,Option[T]] = {
    tree.children.map(buildParentMap(_,Some(tree.label))).reduceLeft(_ ++ _) + (tree.label -> parent);
  }

  val hasUpwardMessage: (T=>Boolean) = TODO

  val children = {
    def buildChildren(tree: Tree[T], parent: Option[T]):Map[T,Set[T]] = {
      val myContribution = (tree.label -> (tree.children.map(_.label).toSet));
      tree.children.map(buildChildren(_,Some(tree.label))).reduceLeft(_ ++ _) + myContribution;
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
      val belief =  if(group.cognatesByT.contains(l)) {
        indicatorBelief(group.cognatesByT(l).word)
      } else  {
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
    override def default(l: T) = if(l == ROOT || l == tree.label) rootMessage else {
      val p = parent(l)
      val messageToP = downwardMessages(p)
      val messages = for(c <- children(p) if hasUpwardMessage(c) && l != c) yield upwardMessages(c)
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

  lazy val likelihood  = {
    val up = upwardBeliefs(tree.label)
    val down = rootMessage;
    up * down partition;
  };

  def edgeMarginal(child: T) = if(hasUpwardMessage(child)) Some(edgeMarginals(child)) else None;
}