package dlwh.cognates;

import Types._;
import scalanlp.util.Log._;

import Factors._;
class InsideOutside(t: Tree, edgeFor: (Language,Language)=>EdgeFactor, initialBeliefs: Language=>Marginal) {
  private val nodes = scala.collection.mutable.Map[Language,Node]();
  private val root = new RootNode(t.asInstanceOf[Ancestor]); // whatever

  def marginalFor(s: Language) = {
    val marg = nodes(s).marginal;
    globalLog.log(DEBUG)(s + marg.fsa);
    marg
  }

  private trait Node {
    def label: Language;
    def marginal: Marginal;
    nodes += (label -> this);
  }

  private trait NonRootNode extends Node {
    def upwardMessage: Marginal
  }

  private trait NonChildNode extends Node {
    def tree: Ancestor;
    val leftChild = nodeFor(tree.lchild);
    val rightChild = nodeFor(tree.rchild);
    def leftMessage: Marginal;
    def rightMessage: Marginal;

    def messageTo(n: Node) = n match {
      case `leftChild` => leftMessage;
      case `rightChild` => rightMessage;
      case _ => throw new Exception("Not one of my children " + n);
    }

    private def nodeFor(t: Tree):NonRootNode = t match {
      case a@Ancestor(l,lc,rc) => new InteriorNode(this,a);
      case c@Child(l) => new ChildNode(l,this);
    }
  }

  private class InteriorNode(parent: NonChildNode, xtree: Ancestor) extends NonChildNode with NonRootNode {
    def label = tree.label;
    def tree = xtree;
    lazy val upwardMessage = {
      edgeFor(parent.label,label).childMarginalize(leftChild.upwardMessage * rightChild.upwardMessage);
    }

    lazy val leftMessage = {
      edgeFor(label,tree.lchild.label).parentMarginalize(rightChild.upwardMessage * parent.messageTo(this));
    }

    lazy val rightMessage = {
      edgeFor(label,tree.rchild.label).parentMarginalize(leftChild.upwardMessage * parent.messageTo(this));
    }

    lazy val marginal = leftChild.upwardMessage * rightChild.upwardMessage * parent.messageTo(this);
  }

  private class RootNode(xtree: Ancestor) extends NonChildNode {
    def label = tree.label;
    def tree = xtree;

    lazy val leftMessage = {
      globalLog.log(DEBUG)((label,tree.lchild.label))
      edgeFor(label,tree.lchild.label).parentMarginalize(rightChild.upwardMessage);
    }

    lazy val rightMessage = {
      edgeFor(label,tree.rchild.label).parentMarginalize(leftChild.upwardMessage);
    }

    lazy val marginal = leftChild.upwardMessage * rightChild.upwardMessage;
  }

  private class ChildNode(val label: Language, parent: NonChildNode) extends NonRootNode {
    lazy val upwardMessage = {
      globalLog.log(DEBUG)("up" + (parent.label,label))
      globalLog.log(DEBUG)(initialBeliefs(label).fsa);
      globalLog.log(DEBUG)("Actual message");
      edgeFor(parent.label,label).childMarginalize(initialBeliefs(label));
    }

    lazy val marginal = initialBeliefs(label) * parent.messageTo(this);
  }

}
