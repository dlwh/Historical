package dlwh.newcognates

import scalanlp.util._;

class TreeInference[F<:Factors](val factors: F, val tree: Tree, val group: CognateGroup) {
  import factors._;
  def parent(l: Language) = parentMap(l);
  private val parentMap = buildParentMap(tree, ROOT)

  private def  buildParentMap(tree: Tree, parent: Language):Map[Language,Language] = tree match {
    case _ : Child => Map(tree.label -> parent);
    case a: Ancestor => a.children.map(buildParentMap(_,a.label)).reduceLeft(_ ++ _) + (a.label -> parent);
  }

  val neighbors = {
    def buildChildren(tree: Tree, parent: Language):Map[Language,Set[Language]] = tree match {
      case _ : Child => Map(tree.label -> Set(parent));
      case a: Ancestor =>
        val myContribution = (a.label -> (a.children.map(_.label).toSet + parent));
        a.children.map(buildChildren(_,a.label)).reduceLeft(_ ++ _) + myContribution;
    }
    buildChildren(tree,"ROOT");
  }
//  println(neighbors);

  val initialBeliefs: BeliefState = {
    val messages = Map.empty ++ (for {
      (l,ns) <- neighbors iterator;
      n <- ns iterator
    } yield {
      val msg = if (n == "ROOT") rootMessage else initialMessage(n, l)
      (n->l) -> msg
    });
    val beliefs = for( l <- parentMap.keys) yield {
      val belief = group.cognates.get(l).map(c => (beliefForWord(c.word)))

      val r = belief getOrElse {
        val ms = for( n <- neighbors(l) iterator) yield messages(n -> l);
        ms.reduceLeft(_ * _ )
      }

      (l -> r)
    };
    new BeliefState(beliefs toMap,messages);
  }

  lazy val onePassBeliefs = initialBeliefs.upward.downward;

  // TODO: make upward better.
  def beliefsAt(l: Language) = initialBeliefs.upward.updateToward(l).belief(l);

  class BeliefState(private[TreeInference] val beliefs: Map[Language,Belief],
                    private[TreeInference] val messages: Map[(Language,Language),Belief]) {
    def likelihood = belief(tree.label).partition;

    private[TreeInference] def message(from: Language, to: Language): Belief = messages(from -> to);

    def belief(language: Language): Belief = {
      if(language == ROOT) rootMessage
      else beliefs(language);
    }

    def edgeMarginal(parent: Language, child: Language):Option[factors.Edge] = {
      if(hasMessage(parent)) Some(new Edge(parent,child).edgeMarginal(this))
      else None;
    };

    def upward:BeliefState = {
      updateOrder.foldLeft(this:BeliefState) { (state,languagePair) =>
        val (parent,child) = languagePair;
        val edge = new Edge(parent,child);
        edge.updateParent(state);
      }
    }

    def downward: BeliefState = {
      updateOrder.foldRight(this:BeliefState) { (languagePair, state) =>
        val (parent,child) = languagePair;
        val edge = new Edge(parent,child);
        edge.updateParent(state);
      }
    }

    def updateToward(l: Language):BeliefState = {
      Iterator.iterate(l)(parentMap).takeWhile(_ != ROOT).grouped(2).foldRight(this) { (langPair, state) =>
        if(langPair.length != 2) state
        else {
          val Seq(child,parent) = langPair;
          val edge = new Edge(parent,child);
          edge.updateChild(state);
        }
      }
    }
  }

  private class Edge(val parent: Language, val child: Language) {
    def updateParent(state: BeliefState) = {
      val (mu_p,mu_c,edgeMarginal) = marginals(state);
      val newMu_p = edgeMarginal.parentProjection;
      val newMsg2p = newMu_p / mu_p;
      val beliefs = state.beliefs.updated(parent,newMu_p);
      val messages = state.messages.updated(child -> parent, newMsg2p);
      new BeliefState(beliefs,messages);
    }

    def isChildObserved = group.cognates.contains(child);

    def updateChild(state: BeliefState) = if(isChildObserved) {
      state
    } else {
      val (_,mu_c,edgeMarginal) = marginals(state);
      val newMu_c = edgeMarginal.childProjection;
      val newMsg2c = newMu_c / mu_c;

      val beliefs = state.beliefs.updated(parent,newMu_c);
      val messages = state.messages.updated(parent -> child, newMsg2c);
      new BeliefState(beliefs,messages);
    }

    def edgeMarginal(state:BeliefState) = {
      marginals(state)._3;
    }

    private def marginals(state:BeliefState) = {
      val mu_p = state.belief(parent);
      val mu_c = state.belief(child);
      val msg2p = state.message(child, parent);
      val msg2c = state.message(parent, child)
      val mu_p_div_c = mu_p / msg2p
      val mu_c_div_p = mu_c / msg2c
      (mu_p,mu_c,edgeFor(parent,child).edgeMarginal(mu_p_div_c, mu_c_div_p));
    }
  }

  private def computeMessageHavers(t: Tree):Set[Language] = {
    if(group.cognates.contains(t.label)) {
      Set(t.label)
    } else t match {
      case t: Child => Set.empty;
      case a: Ancestor =>
        val children = a.children.map(computeMessageHavers _).reduceLeft(_ ++ _);
        if(children.isEmpty) children;
        else children + t.label;
    }
  }

  private val hasMessage = computeMessageHavers(tree);


  private val updateOrder:Seq[(Language,Language)] = {
    // boolean is if there is some language with an observed word below.
    def inferDepth(t: Tree, depth: Int):Seq[((Language,Language),Int)] = {
      if(!hasMessage(t.label)) Seq.empty
      else t match {
        case t: Child => Seq.empty;
        case a: Ancestor =>
          val childrenDepths = a.children.filter(c => hasMessage(c.label)).map(inferDepth(_,depth+1))
          val myEdges = for {
            c <- a.children if hasMessage(c.label)
          } yield (a.label -> c.label) -> depth;
          childrenDepths.reduceLeft(_ ++ _) ++ myEdges;
      }
    }
    val forward = inferDepth(tree,0).toSeq.sortBy(-_._2).map(_._1);
    forward
  }
  println(group, updateOrder);


}
