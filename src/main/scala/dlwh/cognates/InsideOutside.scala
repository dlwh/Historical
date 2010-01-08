package dlwh.cognates;

import Types._;
import scalanlp.util.Log._;

case class InsideOutside[F<:Factors](tree: Tree, 
                         factors: F,
                         bottomWords: Map[Language,Map[Word,Double]]) { outer =>
  import factors._;
                         
  val alphabet = Set.empty ++ (for( map <- bottomWords.valuesIterator;
                      word <- map.keysIterator;
                      ch <- word.iterator
                     ) yield ch);

  private val nodes = scala.collection.mutable.Map[Language,Node]();
  private val root = new RootNode(tree.asInstanceOf[Ancestor]); // whatever

  def cachedUpwardMessage(from: Language): Marginal = nodes(from) match {
    case n:NonRootNode => n.upwardMessage;
    case _ => error("Shouldn'tree be here"); 
  }

  def cachedLeftMessage(from: Language): Marginal = nodes(from) match {
    case n:NonChildNode => n.leftMessage;
    case _ => error("Shouldn'tree be here"); 
  }

  def cachedRightMessage(from: Language): Marginal = nodes(from) match {
    case n:NonChildNode => n.rightMessage;
    case _ => error("Shouldn'tree be here"); 
  }

  def edgeMarginal(from: Language, to: Language) = {
    val f = nodes(from).asInstanceOf[NonChildNode];
    val t = nodes(to).asInstanceOf[NonRootNode];

    val fromMessage = if(t.label == f.leftChild.label) {
      f.leftFactorMessage;
    } else {
      require(t.label == f.rightChild.label);
      f.rightFactorMessage;
    }
    val toMessage = t.upwardFactorMessage;

    val edge = edgeFor(from,to,alphabet);
    edge.withMarginals(fromMessage,toMessage);
  }

  def marginalFor(s: Language) = {
    val marg = nodes(s).marginal;
    marg
  }

  def numOccupants = bottomWords.valuesIterator.map(_.size).toSeq.sum;
  def numOccupants(l: Language) = bottomWords(l).size
  def isEmpty = numOccupants == 0;

  override def toString = {
    "digraph Tree {\n origin -> " + root.label + ";\n" + 
      root.toString + "\n}\n"
  }

  // Include this word as as base word for this language
  def include(language: Language, word: Word, score: Double) = {
    val newBottomWords = bottomWords + (language -> bottomWords(language).+( (word,score))) withDefaultValue(Map.empty);
    if(alphabet !=  (alphabet ++ word))  {
      new InsideOutside(tree,factors,newBottomWords)
    } else {
      assert(score == 0.0);
      val badMessages = tree.predecessorsOfLanguage(language);
      val keep = (Set.empty ++ nodes.keysIterator.filterNot(badMessages));
      InsideOutside.mapped(this, keep, newBottomWords);
    }
  }

  // Remove the word
  def remove(word: Cognate) = {
    val newBottomWords = bottomWords.updated(word.language,bottomWords(word.language) - word.word).withDefaultValue(Map.empty);
    val badMessages = tree.predecessorsOfLanguage(word.language);
    val keep = (Set.empty ++ nodes.keysIterator.filterNot(badMessages));
    InsideOutside.mapped(this, keep, newBottomWords);
  }

  lazy val likelihood = {
    root.likelihood;
  }

  def likelihoodWith(word: Cognate) = {
    val incl = include(word.language,word.word,0.0);
    globalLog.log(INFO)(incl);
    incl.likelihood - likelihood
  }

  private trait Node {
    def label: Language;
    def marginal: Marginal;
    nodes += (label -> this);
  }

  private trait NonRootNode extends Node {
    def upwardMessage: Marginal;
    def hasUpwardMessage: Boolean
    def upwardFactorMessage: Marginal;
  }

  private trait NonChildNode extends Node {
    def tree: Ancestor;
    val leftChild = nodeFor(this,tree.lchild);
    val rightChild = nodeFor(this,tree.rchild);
    def leftMessage: Marginal;
    def rightMessage: Marginal;
    def leftFactorMessage: Marginal;
    def rightFactorMessage: Marginal;

    protected lazy val leftChildHasMessage : Int = if(leftChild.hasUpwardMessage) 1 else 0 
    protected lazy val rightChildHasMessage : Int = if(rightChild.hasUpwardMessage) 1 else 0

    override def toString = {
      val sb = new StringBuffer;
      sb.append(label + " -> " + leftChild.label + ";\n");
      sb.append(label + " -> " + rightChild.label + ";\n");
      sb.append(leftChild.toString);
      sb.append(rightChild.toString);
      sb.toString;
    }


    def messageTo(n: Node) = if(n.label == leftChild.label) {
      cachedLeftMessage(label);
    } else if(n.label == rightChild.label) {
      cachedRightMessage(label);
    } else {
      error("Not one of my ("+tree.label+") children " + n.label + n + leftChild + rightChild);
    }

  }

  private def nodeFor(self: NonChildNode, tree: Tree):NonRootNode = tree match {
    case a@Ancestor(l,lc,rc) => new InteriorNode(self,a);
    case c@Child(l) => new ChildNode(l,self);
  }


  private class InteriorNode(parent: NonChildNode, xtree: Ancestor) extends NonChildNode with NonRootNode {
    def label = tree.label;
    def tree = xtree;
    lazy val hasUpwardMessage = (leftChildHasMessage == 1) || (rightChildHasMessage == 1);

    lazy val upwardFactorMessage = (leftChildHasMessage << 1)|rightChildHasMessage match {
      case 0 => error("Shouldn'tree be calling this if I don'tree have a message");
      case 1 => cachedUpwardMessage(rightChild.label);
      case 2 => cachedUpwardMessage(leftChild.label);
      case 3 => cachedUpwardMessage(rightChild.label) * cachedUpwardMessage(leftChild.label);
      case _ => error("odd. This should be 1, 2, or 3");
    };

    lazy val upwardMessage = {
      assert(hasUpwardMessage);
      edgeFor(parent.label,label,alphabet).childMarginalize(upwardFactorMessage);
    }

    override def toString = {
      val sb = new StringBuffer;
      sb.append(label + " -> " + leftChild.label + ";\n");
      sb.append(label + " -> " + rightChild.label + ";\n");
      sb.append(leftChild.toString);
      sb.append(rightChild.toString);
      sb.toString;
    }

    lazy val leftFactorMessage = {
      val incomingMarg = if(rightChildHasMessage == 1) {
        cachedUpwardMessage(rightChild.label) * parent.messageTo(this);
      } else {
        parent.messageTo(this);
      }
      incomingMarg;
    }

    lazy val leftMessage = {
      val incomingMarg = leftFactorMessage;
      edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(incomingMarg);
    }

    lazy val rightFactorMessage = {
      val incomingMarg = if(leftChildHasMessage == 1) {
        cachedUpwardMessage(leftChild.label) * parent.messageTo(this);
      } else {
        parent.messageTo(this);
      }
      incomingMarg;
    }

    lazy val rightMessage = {
      val incomingMarg = rightFactorMessage;
      edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(incomingMarg);
    }


    lazy val marginal = {
      val parentMessage = parent.messageTo(this);
      (leftChildHasMessage << 1)|rightChildHasMessage match {
        case 0 => parentMessage;
        case 1 => cachedUpwardMessage(rightChild.label) * parentMessage;
        case 2 => cachedUpwardMessage(leftChild.label) * parentMessage;
        case 3 => cachedUpwardMessage(rightChild.label) * cachedUpwardMessage(leftChild.label) * parentMessage;
        case _ => error("odd. This should be 1, 2, or 3");
      };
    }

  }

  private class RootNode(xtree: Ancestor) extends NonChildNode {
    def label = tree.label;
    def tree = xtree;

    val parentMessage = rootMarginal(alphabet);

    def likelihood = marginal.partition;

    lazy val leftFactorMessage = {
      if(rightChild.hasUpwardMessage)
        cachedUpwardMessage(rightChild.label) * parentMessage;
      else (parentMessage);
    }

    lazy val leftMessage = {
      edgeFor(label,tree.lchild.label,alphabet).parentMarginalize(leftFactorMessage);
    }

    lazy val rightFactorMessage = {
      if(leftChild.hasUpwardMessage)
        cachedUpwardMessage(leftChild.label) * parentMessage;
      else (parentMessage);
    }

    lazy val rightMessage = {
      edgeFor(label,tree.rchild.label,alphabet).parentMarginalize(rightFactorMessage);
    }

    lazy val marginal = {
      (leftChildHasMessage << 1)|rightChildHasMessage match {
        case 0 => parentMessage;
        case 1 => cachedUpwardMessage(rightChild.label) * parentMessage;
        case 2 => cachedUpwardMessage(leftChild.label) * parentMessage;
        case 3 => cachedUpwardMessage(rightChild.label) * cachedUpwardMessage(leftChild.label) * parentMessage;
        case _ => error("odd. This should be 1, 2, or 3");
      };
    }

  }

  private class ChildNode(val label: Language, parent: NonChildNode) extends NonRootNode {

    override def toString = {
      val sb = new StringBuffer;
      for( w <- bottomWords(label)) {
        sb.append(label + " -> " + w._1 + ";\n");
      }
      sb.toString;
    }

    lazy val hasUpwardMessage = bottomWords.get(label).map(!_.isEmpty).getOrElse(false);

    lazy val upwardFactorMessage = {
      assert(hasUpwardMessage);
      assert(bottomWords(label).size == 1);
      val (word,score) = bottomWords(label).iterator.next;
      marginalForWord(word,score);
    }

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

object InsideOutside {
  def mapped[F<:Factors](current: InsideOutside[F],
    keepFactors: Set[Language],
    newBottomWords: Map[Language,Map[Word,Double]]) = {
    new InsideOutside[F](current.tree,current.factors,newBottomWords) {
      import factors._;
      override def cachedUpwardMessage(from: Language): Marginal = if(keepFactors(from)) {
        current.cachedUpwardMessage(from).asInstanceOf[factors.Marginal];
      } else {
        super.cachedUpwardMessage(from);
      }

      override def cachedLeftMessage(from: Language): Marginal = if(keepFactors(from)) {
        current.cachedLeftMessage(from).asInstanceOf[factors.Marginal];
      } else {
        super.cachedLeftMessage(from);
      }

      override def cachedRightMessage(from: Language): Marginal = if(keepFactors(from)) {
        current.cachedRightMessage(from).asInstanceOf[factors.Marginal];
      } else {
        super.cachedRightMessage(from);
      }
    }
  }
}
