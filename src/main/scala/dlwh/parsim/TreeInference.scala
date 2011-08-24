package dlwh.parsim

import dlwh.cognates._
import phylo.Tree
import collection.mutable

/**
 * 
 * @author dlwh
 */

class TreeInference[F<:Factors[Language]](val factors: F, val tree: Tree[Language], val group: CognateGroup) {
  import factors._;
  def parent(l: Language) = parentMap(l);
  private val parentMap = buildParentMap(tree, ROOT)

  private def  buildParentMap(tree: Tree[Language], parent: Language):Map[Language,Language] = {
    tree.children.map(buildParentMap(_,tree.label)).reduceLeft(_ ++ _) + (tree.label -> parent);
  }

  val hasUpwardMessage = group.nodesWithObservedDescendants(tree);

  val children = {
    def buildChildren(tree: Tree[Language], parent: Language):Map[Language,Set[Language]] = {
      val myContribution = (tree.label -> (tree.children.map(_.label).toSet));
      tree.children.map(buildChildren(_,tree.label)).reduceLeft(_ ++ _) + myContribution;
    }
    buildChildren(tree,ROOT);
  }

  val upwardMessages:collection.mutable.Map[Language,Belief] = new collection.mutable.HashMap[Language,Belief] {
    override def default(l: Language) = {
      val b:Belief = upwardBeliefs(l);
      val message = edgeFor(l).edgeMarginal(uniformBelief,b).parentProjection;
      update(l,message);
      message
    }
  }

  val upwardBeliefs: collection.mutable.Map[Language,Belief] = new collection.mutable.HashMap[Language,Belief] {
    override def default(l: Language) = {
      val belief =  if(group.cognatesByLanguage.contains(l)) {
        indicatorBelief(group.cognatesByLanguage(l).word)
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
  val downwardMessages: collection.mutable.Map[Language,Belief] = new collection.mutable.HashMap[Language,Belief] {
    override def default(l: Language) = if(l == ROOT || l == tree.label) rootMessage else {
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

  val edgeMarginals:collection.mutable.Map[Language,EdgeMarginal] = new collection.mutable.HashMap[Language,EdgeMarginal] {
    override def default(l: Language) = {
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

  def edgeMarginal(child: Language) = if(hasUpwardMessage(child)) Some(edgeMarginals(child)) else None;
}