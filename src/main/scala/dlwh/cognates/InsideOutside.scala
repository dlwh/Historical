package dlwh.cognates;

import Types._;
import scalanlp.util.Log._;

/** InsideOutside is the wrong name. This does message passing on phylogenetic trees with words
   optionally observed */
class InsideOutside[F<:Factors](tree: Tree, val factors: F, bottomWords: Map[Language,Cognate]=Map.empty) {
  import factors._;
  val alphabet = Set.empty ++ (for( word <- bottomWords.valuesIterator;
                      ch <- word.word.iterator
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
  def include(c: Cognate) = {
    new InsideOutside(tree,factors,bottomWords + (c.language->c));
  }

  // Remove the word associated with this language
  def remove(language: Language) = {
    new InsideOutside(tree, factors, bottomWords - language);
  }


  // Tries to merge the languages of two IOs together. If they have overlapping elements, it gives up.
  def merge(io2: InsideOutside[F]) = {
    if(words.keys.exists(io2.words.keySet) ) // is there any overlap in the languages?
      None
    else {
      val newWords = words ++ io2.words;
      Some(new InsideOutside(tree, factors, newWords));
    }
  }

  def wordForLanguage(language:Language):Option[Cognate] = bottomWords.get(language);
  def words = bottomWords

  lazy val likelihood = {
    root.likelihood;
  }

  def likelihoodWith(word: Cognate) = {
    val incl = include(word);
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
        math.log(prob) + recursiveTreeScore(deathScores,edge.child);
      } else {
        val death = math.log(deathScores(root.language -> edge.child.language));
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
      System.out.println("Up from node " + language);
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
      System.out.println("From Node " + language + " to edge " + to.child.language)
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
      val word = bottomWords.get(label).map(_.word);
      val node = new Node(label,word,Seq.empty);
      node
    case Ancestor(label,treeChildren) =>
      val children = treeChildren.map(buildTree);
      val word = bottomWords.get(label).map(_.word);
      val node:Node = new Node(label, word, children map {n =>
          val e:Edge = new Edge(n);
          n.parentMessage = Some( ()=>e.downwardMessage);
          e
      });
      node.children.foreach { _.parent = node};
      node
  }

}

