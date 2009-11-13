package dlwh.cognates;

import Types._;
import scalanlp.util.Log._;

case class InsideOutside(t: Tree, 
                         f: Factors,
                         bottomWords: Map[Language,Map[Word,Double]]) {
  import f._;

  val alphabet = Set.empty ++ (for( map <- bottomWords.valuesIterator;
                      word <- map.keysIterator;
                      ch <- word.iterator
                     ) yield ch);

  private val nodes = scala.collection.mutable.Map[Language,Node]();
  private val root = new RootNode(t.asInstanceOf[Ancestor]); // whatever

  override def toString = {
    "digraph Tree {\n origin -> " + root.label + "[label="+rootMarginal(alphabet).partition+"];\n" +
      root.toString + "\n}\n"
  }

  def marginalFor(s: Language) = {
    val marg = nodes(s).marginal;
    marg
  }

  // Include this word as as base word for this language
  def include(language: Language, word: Word, score: Double) = {
    assert(score == 0.0);
    val newBottomWords = bottomWords + (language -> bottomWords(language).+( (word,score))) withDefaultValue(Map.empty);
    this.copy(bottomWords = newBottomWords);
  }

  // Remove the word
  def remove(word: Cognate) = {
    val newBottomWords = bottomWords.updated(word.language,bottomWords(word.language) - word.word).withDefaultValue(Map.empty);
    this.copy(bottomWords = newBottomWords);
  }

  lazy val likelihood = {
    root.likelihood;
  }

  def likelihoodWith(word: Cognate) = {
    val incl = include(word.language,word.word,0.0);
    incl.likelihood - likelihood
  }

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

    protected lazy val leftChildHasMessage : Int = if(leftChild.hasUpwardMessage) 1 else 0 
    protected lazy val rightChildHasMessage : Int = if(rightChild.hasUpwardMessage) 1 else 0

    override def toString = {
      val sb = new StringBuffer;
      sb.append(label + " -> " + leftChild.label + "[label=" + leftMessage.partition +"];\n");
      sb.append(label + " -> " + rightChild.label + "[label=" + rightMessage.partition +"];\n");
      sb.append(leftChild.toString);
      sb.append(rightChild.toString);
      sb.toString;
    }


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

    lazy val upwardMessage = {
      assert(hasUpwardMessage);
      val incomingMarg = (leftChildHasMessage << 1)|rightChildHasMessage match {
        case 0 => error("Shouldn't be calling this if I don't have a message");
        case 1 => rightChild.upwardMessage;
        case 2 => leftChild.upwardMessage;
        case 3 => rightChild.upwardMessage * leftChild.upwardMessage;
        case _ => error("odd. This should be 1, 2, or 3");
      };
      edgeFor(parent.label,label,alphabet).childMarginalize(incomingMarg);
    }

    override def toString = {
      val sb = new StringBuffer;
      sb.append(label + " -> " + leftChild.label + "[label=" + leftMessage.partition +"];\n");
      sb.append(label + " -> " + rightChild.label + "[label=" + rightMessage.partition +"];\n");
      if(hasUpwardMessage)
        sb.append(label + " -> " + parent.label + "[label=" + upwardMessage.partition +"];\n");
      else
        sb.append(label + " -> " + parent.label + "[label=nothing];\n");
      sb.append(leftChild.toString);
      sb.append(rightChild.toString);
      sb.toString;
    }

    lazy val leftMessage = {
      val incomingMarg = if(rightChildHasMessage == 1) {
        rightChild.upwardMessage * parent.messageTo(this);
      } else {
        parent.messageTo(this);
      }
      edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(incomingMarg);
    }

    lazy val rightMessage = {
      val incomingMarg = if(leftChildHasMessage == 1) {
        leftChild.upwardMessage * parent.messageTo(this);
      } else {
        parent.messageTo(this);
      }
      edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(incomingMarg);
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

    val parentMessage = rootMarginal(alphabet);

    def likelihood = marginal.partition;

    lazy val leftMessage = {
      globalLog.log(DEBUG)(("Root lm",label,tree.lchild.label))
      if(rightChild.hasUpwardMessage)
        edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(rightChild.upwardMessage * parentMessage);
      else edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(parentMessage);
    }

    lazy val rightMessage = {
      globalLog.log(DEBUG)(("Root rm",label,tree.rchild.label))
      if(leftChild.hasUpwardMessage)
        edgeFor(label,tree.rchild.label,alphabet).parentMarginalize(leftChild.upwardMessage * parentMessage);
      else edgeFor(label,tree.rchild.label,alphabet).parentMarginalize(parentMessage);
    }

    lazy val marginal = {
      (leftChildHasMessage << 1)|rightChildHasMessage match {
        case 0 => parentMessage;
        case 1 => rightChild.upwardMessage * parentMessage;
        case 2 => leftChild.upwardMessage * parentMessage;
        case 3 => rightChild.upwardMessage * leftChild.upwardMessage * parentMessage;
        case _ => error("odd. This should be 1, 2, or 3");
      };
    }

  }

  private class ChildNode(val label: Language, parent: NonChildNode) extends NonRootNode {

    override def toString = {
      val sb = new StringBuffer;
      if(hasUpwardMessage)
        sb.append(label + " -> " + parent.label + "[label=" + upwardMessage.partition +"];\n");
      else
        sb.append(label + " -> " + parent.label + "[label=nothing];\n");
      for( w <- bottomWords(label)) {
        sb.append(label + " -> " + w._1 + ";\n");
      }
      sb.toString;
    }

    lazy val hasUpwardMessage = bottomWords.get(label).map(!_.isEmpty).getOrElse(false);

    lazy val upwardMessage = {
      globalLog.log(DEBUG)("up" + (parent.label,label))
      globalLog.log(DEBUG)(bottomWords(label));
      val edge = edgeFor(parent.label,label,alphabet);

      val message = bottomWords.getOrElse(label,Map.empty).iterator map { case (word,score) =>
        val wordMarginal = marginalForWord(word,score);
        edge.childMarginalize(wordMarginal)
      } reduceLeft (_*_);

      message;
    }

    lazy val marginal ={
      val marg = parent.messageTo(this);
      marg
    }
  }

}
