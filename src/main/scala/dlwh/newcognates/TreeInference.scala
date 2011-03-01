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

  lazy val onePassBeliefs = initialBeliefs.update;

  def beliefsAt(l: Language) = onePassBeliefs.updateToward(l).belief(l);

  class BeliefState(private[TreeInference] val beliefs: Map[Language,Belief],
                    private[TreeInference] val messages: Map[(Language,Language),Belief]) {
    def likelihood = belief(tree.label).partition;

    private[TreeInference] def message(from: Language, to: Language): Belief = messages(from -> to);

    def belief(language: Language): Belief = {
      if(language == ROOT) rootMessage
      else beliefs(language);
    }

    def update:BeliefState = {
      updateOrder.foldLeft(this:BeliefState) { (state,languagePair) =>
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
//      println(child + " upto " + parent);
      val mu_p = state.belief(parent);
//      println("mu_p" + " " + mu_p);
      val mu_c = state.belief(child);
//      println("mu_c" + " " + mu_c);
      val msg2p = state.message(child, parent);
//      println("msg2p" + " " + msg2p);
      val msg2c = state.message(parent, child)
//      println("msg2c" + " " + msg2c);
      val mu_p_div_c = mu_p / msg2p
//      println("p / c" + " " + mu_p_div_c);
      val mu_c_div_p = mu_c / msg2c
//      println("c / p" + " " + mu_c_div_p);
      val newMu_p = projectToParent(mu_p_div_c, mu_c_div_p, edgeFor(parent,child));
      val newMsg2p = newMu_p / mu_p;

      val beliefs = state.beliefs.updated(parent,newMu_p);
      val messages = state.messages.updated(child -> parent, newMsg2p);
      new BeliefState(beliefs,messages);
    }

    def isChildObserved = group.cognates.contains(child);

    def updateChild(state: BeliefState) = if(isChildObserved) {
      state
    } else {
//      println(parent + " downto " + child);
      val mu_p = state.belief(parent);
//      println("mu_p" + " " + mu_p);
      val mu_c = state.belief(child);
//      println("mu_c" + " " + mu_c);
      val msg2p = state.message(child, parent);
//      println("msg2p" + " " + msg2p);
      val msg2c = state.message(parent, child)
//      println("msg2c" + " " + msg2c);
      val mu_p_div_c = mu_p / msg2p
//      println("p / c" + " " + mu_p_div_c);
      val mu_c_div_p = mu_c / msg2c
//      println("c / p" + " " + mu_c_div_p);
      val newMu_c = projectToChild(mu_p_div_c, mu_c_div_p, edgeFor(parent,child));
      val newMsg2c = newMu_c / mu_c;

      val beliefs = state.beliefs.updated(parent,newMu_c);
      val messages = state.messages.updated(parent -> child, newMsg2c);
      new BeliefState(beliefs,messages);
    }
  }


  private val updateOrder:Seq[(Language,Language)] = {
    // boolean is if there is some language with an observed word below.
    def inferDepth(t: Tree, depth: Int):(Boolean,Map[(Language,Language),Int]) = {
      t match {
        case t: Child => (group.cognates.contains(t.label),Map.empty[(Language,Language),Int]);
        case a: Ancestor =>
          val childrenDepths = a.children.map(inferDepth(_,depth+1))
          if(childrenDepths.exists(_._1)) {
            val myEdges = for {
              (child,(hasSomeObservedChild,_)) <- a.children zip childrenDepths if hasSomeObservedChild
            } yield (t.label -> child.label) -> depth;
            (true,childrenDepths.view.filter(_._1).map(_._2).reduceLeft(_ ++ _) ++ myEdges);
          }
          else (false,Map.empty[(Language,Language),Int])
      }
    }
    val forward = inferDepth(tree,0)._2.toSeq.sortBy(-_._2).map(_._1);
    forward
  }
  println(group, updateOrder);


}
