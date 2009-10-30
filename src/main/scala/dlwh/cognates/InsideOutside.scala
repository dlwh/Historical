package dlwh.cognates;

import Types._;
import scalanlp.util.Log._;

import Factors._;
case class InsideOutside(t: Tree, rootMarginal: Marginal,
                         edgeFor: (Language,Language)=>EdgeFactor,
                         bottomWords: Map[Language,Map[Word,Double]]) {
  private val nodes = scala.collection.mutable.Map[Language,Node]();
  private val root = new RootNode(t.asInstanceOf[Ancestor]); // whatever

  def marginalFor(s: Language) = {
    val marg = nodes(s).marginal;
    globalLog.log(DEBUG)(s + marg.fsa);
    marg
  }

  // Include this word as as base word for this language
  def include(language: Language, word: Word, score: Double) = {
    assert(score == 0.0);
    val newBottomWords = bottomWords + (language -> bottomWords(language).+( (word,score))) withDefaultValue(Map.empty);
    this.copy(bottomWords = newBottomWords);
  }

  // Remove the word
  def remove(language: Language, word: Word) = {
    val newBottomWords = bottomWords.updated(language,bottomWords(language) - word).withDefaultValue(Map.empty);
    this.copy(bottomWords = newBottomWords);
  }

  lazy val likelihood = {
    nodes.valuesIterator.map { 
      case c:ChildNode => c.likelihood 
      case x =>  0.0:Double
    }.foldLeft[Double](0.0)( _ + _ );
  }

  def likelihoodWith(word: Cognate) = include(word.language,word.word,0.0).likelihood - likelihood;

  private trait Node {
    def label: Language;
    def marginal: Marginal;
    nodes += (label -> this);
  }

  private trait NonRootNode extends Node {
    def upwardMessage: Marginal
    def hasUpwardMessage: Boolean
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
      case _ => {
        throw new Exception("Not one of my ("+tree.label+") children " + n.label + n + leftChild + rightChild);
      }
    }

    private def nodeFor(t: Tree):NonRootNode = t match {
      case a@Ancestor(l,lc,rc) => new InteriorNode(this,a);
      case c@Child(l) => new ChildNode(l,this);
    }
  }

  private class InteriorNode(parent: NonChildNode, xtree: Ancestor) extends NonChildNode with NonRootNode {
    def label = tree.label;
    def tree = xtree;
    lazy val hasUpwardMessage = (leftChildHasMessage == 1) || (rightChildHasMessage == 1);
    private lazy val leftChildHasMessage : Int = leftChild.hasUpwardMessage.asInstanceOf[Int];
    private lazy val rightChildHasMessage : Int = rightChild.hasUpwardMessage.asInstanceOf[Int];

    lazy val upwardMessage = {
      assert(hasUpwardMessage);
      val incomingMarg = (leftChildHasMessage << 1)|rightChildHasMessage match {
        case 0 => error("Shouldn't be calling this if I don't have a message");
        case 1 => rightChild.upwardMessage;
        case 2 => leftChild.upwardMessage;
        case 3 => rightChild.upwardMessage * leftChild.upwardMessage;
        case _ => error("odd. This should be 1, 2, or 3");
      };
      edgeFor(parent.label,label).childMarginalize(incomingMarg);
    }

    lazy val leftMessage = {
      val incomingMarg = if(rightChildHasMessage == 1) {
        rightChild.upwardMessage * parent.messageTo(this);
      } else {
        parent.messageTo(this);
      }
      edgeFor(label,tree.lchild.label).parentMarginalize(incomingMarg);
    }

    lazy val rightMessage = {
      val incomingMarg = if(leftChildHasMessage == 1) {
        leftChild.upwardMessage * parent.messageTo(this);
      } else {
        parent.messageTo(this);
      }
      edgeFor(label,tree.lchild.label).parentMarginalize(incomingMarg);
    }

    lazy val marginal = {
      val parentMessage = parent.messageTo(this);
      (leftChildHasMessage << 1)|rightChildHasMessage match {
        case 0 => parentMessage;
        case 1 => rightChild.upwardMessage * parentMessage;
        case 2 => leftChild.upwardMessage * parentMessage;
        case 3 => rightChild.upwardMessage * leftChild.upwardMessage * parentMessage;
        case _ => error("odd. This should be 1, 2, or 3");
      };
    }
  }

  private class RootNode(xtree: Ancestor) extends NonChildNode {
    def label = tree.label;
    def tree = xtree;

    val parentMessage = rootMarginal;

    lazy val leftMessage = {
      globalLog.log(DEBUG)(("Root lm",label,tree.lchild.label))
      if(rightChild.hasUpwardMessage)
        edgeFor(label,tree.lchild.label).parentMarginalize(rightChild.upwardMessage * parentMessage);
      else edgeFor(label,tree.lchild.label).parentMarginalize(parentMessage);
    }

    lazy val rightMessage = {
      globalLog.log(DEBUG)(("Root rm",label,tree.rchild.label))
      if(leftChild.hasUpwardMessage)
        edgeFor(label,tree.rchild.label).parentMarginalize(leftChild.upwardMessage * parentMessage);
      else edgeFor(label,tree.rchild.label).parentMarginalize(parentMessage);
    }

    lazy val marginal = leftChild.upwardMessage * rightChild.upwardMessage;
  }

  private class ChildNode(val label: Language, parent: NonChildNode) extends NonRootNode {
    lazy val hasUpwardMessage = bottomWords.get(label).map(!_.isEmpty).getOrElse(false);

    lazy val upwardMessage = {
      globalLog.log(DEBUG)("up" + (parent.label,label))
      globalLog.log(DEBUG)(bottomWords(label));
      val edge = edgeFor(parent.label,label);

      val message = bottomWords.getOrElse(label,Map.empty).iterator map { case (word,score) =>
        val const = new Marginal(word,score);
        edge.childMarginalize(const)
      } reduceLeft (_*_);

      globalLog.log(DEBUG)("Actual message" + message.fsa);
      message;
    }

    lazy val likelihood:Double = { 
      bottomWords.getOrElse(label,Map.empty).iterator.map { case (word,score) =>
        // i.e. assignment must be hard. It doesn't make sense otherwise
        marginal(word);
      }.foldLeft(0.0)(_ + _);
    }

    lazy val marginal = parent.messageTo(this);
  }

}
