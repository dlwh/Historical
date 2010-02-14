package dlwh.cognates;

import Types._;
import scalanlp.util.Log._;

class InsideOutside[F<:Factors](tree: Tree, val factors: F, bottomWords: Map[Language,Word]=Map.empty) {
  import factors._;
  val alphabet = Set.empty ++ (for( word <- bottomWords.valuesIterator;
                      ch <- word.iterator
                     ) yield ch)

  def edgeMarginal(from: Language, to: Language): Option[EdgeFactor] = {
    edges.get( (from,to)).filter(_.hasUpwardMessage).map(_.edgeMarginal);
  }

  def marginalFor(s: Language):Option[Marginal] = {
    nodes.get(s).map(_.marginal);
  }

  def reconstruction(s: Language):Option[Marginal] = {
    nodes.get(s).map(_.reconstructedMarginal);
  }

  // Include this word as as base word for this language
  def include(language: Language, word: Word) = {
    new InsideOutside(tree,factors,bottomWords + ((language,word)));
  }

  // Remove the word associated with this language
  def remove(language: Language) = {
    new InsideOutside(tree,factors,bottomWords - language);
  }

  def wordForLanguage(language:Language):Option[Word] = bottomWords.get(language);

  lazy val likelihood = {
    root.likelihood;
  }

  def assignments = bottomWords.map{ case(l,v) => Cognate(l,v)};

  def likelihoodWith(word: Cognate) = {
    val incl = include(word.language,word.word);
    incl.likelihood - likelihood
  }

  def isEmpty = bottomWords.isEmpty;
  def numOccupants = bottomWords.size;
  def numOccupants(l: Language) = if(bottomWords.contains(l)) 1 else 0;

  private val root = buildTree(tree);
  def rootMarginal = root.marginal;
  root.parentMessage = Some(() => factors.rootMarginal(alphabet));

  def pathsToFringe : Seq[((Language,Language),Boolean)] = recursivePaths(root);

  private def recursivePaths(root: Node):Seq[((Language,Language),Boolean)]  = {
    val children = root.children.flatMap { edge =>
      if(edge.hasUpwardMessage) {
        Seq(((edge.parent.language,edge.child.language),false)) ++ recursivePaths(edge.child);
      } else {
        Seq(((edge.parent.language,edge.child.language),true))
      }
    }
    children;
  }

  def treePrior(deathScores: Map[(Language,Language),Double]) = recursiveTreeScore(deathScores,root);

  private def recursiveTreeScore(deathScores: Map[(Language,Language),Double], root: Node):Double = {
    val childScores = root.children.map { edge =>
      if(edge.hasUpwardMessage) {
        val prob = 1.0 - deathScores(root.language -> edge.child.language);
        Math.log(prob) + recursiveTreeScore(deathScores,edge.child);
      } else {
        val death = Math.log(deathScores(root.language -> edge.child.language));
        death
      }
    }
    childScores.foldLeft(0.0)(_+_);
  }

  private lazy val nodes = root.descendentNodes;
  private lazy val edges = root.descendentEdges;

  private class Edge(val child: Node) {
    var parent:Node = _;
    def edgeMarginal:EdgeFactor = myEdge.withMarginals(incomingParentMessage, child.upwardMessage);

    def hasUpwardMessage = child.hasUpwardMessage;
    lazy val upwardMessage:Marginal = myEdge.childMarginalize(child.upwardMessage);
    lazy val downwardMessage: Marginal = myEdge.parentMarginalize(incomingParentMessage);

    private lazy val incomingParentMessage = parent.downwardMessage(this);
    private lazy val myEdge = edgeFor(parent.language,child.language,alphabet);
  }

  private case class Node(language: Language, word: Option[Word], children: Seq[Edge], var parentMessage: Option[()=>Marginal]=None) {
    def descendentNodes:Map[Language,Node] = {
      Map.empty + ((language,this)) ++ children.flatMap(_.child.descendentNodes);
    }

    def descendentEdges:Map[(Language,Language),Edge] = {
      val myEdges = children.map ( e => ((language,e.child.language),e));
      val childEdges = children.flatMap(_.child.descendentEdges);
      Map.empty ++ childEdges ++ myEdges;
    }

    lazy val marginal: Marginal = {
      val parent = parentMessage.get.apply();
      val incoming: Seq[Marginal] = children.iterator.withFilter(_.hasUpwardMessage).map(_.upwardMessage).toSeq
      val wordMessage  = word.iterator.map(marginalForWord(_,0.0));
      if(word.isEmpty) product(false,Seq(parent) ++ incoming);
      else {
        val inc = product(false,Seq(parent) ++ incoming);
        val cost = inc(word.get);
        val m = marginalForWord(word.get,cost);
        m
      }
    }

    def hasUpwardMessage:Boolean = !word.isEmpty || children.exists(_.hasUpwardMessage);

    lazy val upwardMessage:Marginal = {
      assert(hasUpwardMessage);
      val incoming = children.iterator.withFilter(_.hasUpwardMessage).map(_.upwardMessage).toSeq;
      if(children.isEmpty) marginalForWord(word.get)
      else if(word.isEmpty) product(true,incoming);
      else {
        val inc = product(true,incoming);
        val cost = inc(word.get);
        val m = marginalForWord(word.get,cost);
        m
      }
    }

    lazy val reconstructedMarginal: Marginal = {
      val incoming = parentMessage.iterator.map(_.apply()) ++ children.iterator.filter(_.hasUpwardMessage).map(_.upwardMessage);
      product(false,incoming.toSeq);
    }

    lazy val likelihood: Double = marginal.partition;

    def downwardMessage(to: Edge):Marginal = {
      val parent = parentMessage.iterator.map(_.apply());
      val outgoingChildren = children.iterator.filter(c => to != c && c.hasUpwardMessage).map(_.upwardMessage);
      val nonWordMessages = parent ++ outgoingChildren;
      val marg = if(!nonWordMessages.hasNext) marginalForWord(word.get,0.0)
      else if(word.isEmpty) product(false,nonWordMessages.toSeq);
      else {
        val r = product(false,nonWordMessages.toSeq);
        val newCost = r(word.get);
        marginalForWord(word.get,newCost);
      }
      marg
    }
  }

  private def buildTree(tree: Tree): Node = tree match {
    case Child(label) => 
      val word = bottomWords.get(label)
      val node = new Node(label,word,Seq.empty);
      node
    case Ancestor(label,treeChildren) =>
      val children = treeChildren.map(buildTree);
      val word = bottomWords.get(label);
      val node:Node = new Node(label, word, children map {n =>
          val e:Edge = new Edge(n);
          n.parentMessage = Some( ()=>e.downwardMessage);
          e
      });
      node.children.foreach { _.parent = node};
      node
  }

}

/*

class InsideOutside2[F<:Factors](tree: Tree,
                         val factors: F,
                         bottomWords: Map[Language,Map[Word,Double]]) { outer =>
  import factors._;
                         
  val alphabet = Set.empty ++ (for( map <- bottomWords.valuesIterator;
                      word <- map.keysIterator;
                      ch <- word.iterator
                     ) yield ch);

  private val nodes = scala.collection.mutable.Map[Language,Node]();
  private val root = new RootNode(tree.asInstanceOf[Ancestor]); // whatever

  def cachedUpwardMessage(from: Language): Marginal = nodes(from) match {
    case n:NonRootNode =>  val r = n.upwardMessage;  r
    case _ => error("Shouldn't be here"); 
  }

  def cachedLeftMessage(from: Language): Marginal = nodes(from) match {
    case n:NonChildNode => n.leftMessage;
    case _ => error("Shouldn't be here"); 
  }

  def cachedRightMessage(from: Language): Marginal = nodes(from) match {
    case n:NonChildNode => n.rightMessage;
    case _ => error("Shouldn't be here"); 
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
    val edge = edgeFor(from,to,alphabet);
    val toMessage = t.upwardFactorMessage;
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
    val newBottomWords = bottomWords + Tuple2(language,  bottomWords(language).+( (word,score))) withDefaultValue(Map.empty);
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
    nodes += Tuple2(label,this);
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

*/
