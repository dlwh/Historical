package dlwh.newcognates

import scalanlp.util._;

abstract class TreeInference[F<:Factors](val factors: F, val tree: Tree, val group: CognateGroup) {
  import factors._;
  def parent(l: Language) = parentMap(l);
  private val parentMap = buildParentMap(tree, "<ROOT>")

  private def  buildParentMap(tree: Tree, parent: Language):Map[Language,Language] = tree match {
    case _ : Child => Map(tree.label -> parent);
    case a: Ancestor => a.children.map(buildParentMap(_,a.label)).reduceLeft(_ ++ _) + (a.label -> parent);
  }


  val initialBeliefs: BeliefState = new BeliefState {
    override private[TreeInference] val nodes = buildBeliefs(tree)
  }

  trait BeliefState {
    private[TreeInference] val nodes : Map[Language,Node];
    def belief(language: Language) = {
      if(language == "<ROOT>") rootMessage
      else nodes(language).belief;
    }

    def update:BeliefState = {
      updateOrder.foldLeft(this) { (state,language) =>
        val node = nodes(language);
        val newNodes = state.nodes.updated(language,node.update(state));
        new BeliefState {
          private[TreeInference] val nodes : Map[Language,Node] = newNodes;
        }
      }
    }

  }

  private sealed trait Node {
    def language: Language
    def belief: Belief
    def update(state: BeliefState):Node
  }

  private class UnobservedNode(tree: Tree,
                       _belief: =>Belief) extends Node {
    lazy val belief = _belief;
    def language = tree.label;
    def update(state: BeliefState) = {
      def newBelief = {
        val parentBelief = state.belief(parent(language))
        val parentMessage = sendMessageToChild(parentBelief,parent(language),language);
        val childrenMessages = tree match {
          case _ : Child => Seq.empty;
          case a: Ancestor => a.children.map { c =>
            val belief = state.belief(c.label);
            sendMessageToParent(belief,c.label, language);
          }
        }
        product(parentMessage +: childrenMessages)
      }
      new UnobservedNode(tree, newBelief);
    }
  }
  private class ObservedNode(val language: Language, val word: Word) extends Node {
    val belief = beliefForWord(word);
    def update(state: BeliefState) = this;
  }


  private val updateOrder:Seq[Language] = {
    def inferDepth(t: Tree, depth: Int):Map[Language,Int] = {
      val children = if(group.cognates.contains(t.label)) Map.empty[Language,Int]
      else {
        val rest = t match {
          case t: Child => Map.empty[Language,Int];
          case a: Ancestor => a.children.map(inferDepth(_,depth+1)).reduceLeft(_ ++ _)
        }
        rest
      }
      children + (t.label -> depth);
    }
    val forward = inferDepth(tree,0).toSeq.sortBy(-_._2).map(_._1);
    val backward = forward.reverse
    forward ++ backward;
  }
  println(updateOrder);


  private def buildBeliefs(tree: Tree):Map[Language,Node] = {
    val myNode = group.cognates.get(tree.label).map( c => new ObservedNode(tree.label,c.word)) getOrElse {
      new UnobservedNode(tree,initialBelief(tree.label))
    };
    val otherNodes = tree match {
      case a: Ancestor =>
        a.children.map(buildBeliefs(_)) reduceLeft(_++_);
      case _ => Map.empty[Language,Node]
    }

    otherNodes + (tree.label -> myNode)
  }

}
