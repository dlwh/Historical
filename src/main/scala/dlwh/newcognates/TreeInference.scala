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

  val initialBeliefs: BeliefState = {
    val beliefs = {
      val m = Map[Language,Lazy[Belief]]().withDefault( l => Lazy(initialBelief(l)))
      m ++ group.cognates.mapValues{c => val b = beliefForWord(c.word); Lazy(b)};
    };
    val messages = Map[(Language,Language),Lazy[Belief]]().withDefault { case (from,to) =>
      Lazy(initialMessage(from,to))
    };
    new BeliefState(beliefs,messages);
  }

  lazy val onePassBeliefs = initialBeliefs.update;

  def beliefsAt(l: Language) = onePassBeliefs.updateToward(l).belief(l);

  class BeliefState(private[TreeInference] val beliefs: Map[Language,Lazy[Belief]],
                    private[TreeInference] val messages: Map[(Language,Language),Lazy[Belief]]) {
    def likelihood = belief(tree.label).partition;
    def belief(language: Language):Belief = {
      lazyBelief(language).result
    }

    private[TreeInference] def lazyMessage(from: Language, to: Language): Lazy[Belief] = messages(from -> to);

    private[TreeInference] def lazyBelief(language: Language): Lazy[Belief] = {
      if(language == ROOT) Lazy(rootMessage)
      else beliefs(language);
    }

    def update:BeliefState = {
      updateOrder.foldLeft(this:BeliefState) { (state,languagePair) =>
        val (parent,child) = languagePair;
        val edge = if(group.cognates.contains(child)) new ObservedEdge(parent,child) else new UnobservedEdge(parent,child);
        edge.update(state);
      }
    }

    def updateToward(l: Language):BeliefState = {
      Iterator.iterate(l)(parentMap).takeWhile(_ != ROOT).grouped(2).foldRight(this) { (langPair, state) =>
        if(langPair.length != 2) state
        else {
          val Seq(child,parent) = langPair;
          val edge = if(group.cognates.contains(child)) new ObservedEdge(parent,child) else new UnobservedEdge(parent,child);
          edge.update(state);
        }
      }
    }
  }

  private sealed trait Edge {
    def parent: Language
    def child :Language
    def update(state: BeliefState):BeliefState;
  }

  private class UnobservedEdge(val parent: Language, val child: Language) extends Edge {
    def update(state: BeliefState) = {
      case class UpdateBundle(newParentBelief: Lazy[Belief],
                                      newChildBelief: Lazy[Belief],
                                      newP2CMessage: Lazy[Belief],
                                      newC2PMessage: Lazy[Belief]);

      val lazyBundle = for {
        mu_p <- state.lazyBelief(parent);
        mu_c <- state.lazyBelief(child);
        msg2p <- state.lazyMessage(child, parent);
        msg2c <- state.lazyMessage(parent, child)
      } yield {
        val mu_p_div_c = mu_p / msg2p
        val mu_c_div_p = mu_c / msg2c
        val (lazyNewMu_p, lazyNewMu_c) = projectMessages(mu_p_div_c, mu_c_div_p, edgeFor(parent,child));
        val newMsg2p = for (newMu_p <- lazyNewMu_p) yield newMu_p / mu_p;
        val newMsg2c = for (newMu_c <- lazyNewMu_c) yield newMu_c / mu_c;
        UpdateBundle(lazyNewMu_p, lazyNewMu_c, newMsg2c, newMsg2p);
      }

      val beliefs = (state.beliefs
        .updated(parent, lazyBundle.flatMap(_.newParentBelief))
        .updated(child, lazyBundle.flatMap(_.newChildBelief)));
      val messages = (state.messages
        .updated(parent -> child, lazyBundle.flatMap(_.newP2CMessage))
        .updated(child -> parent, lazyBundle.flatMap(_.newC2PMessage)));

      new BeliefState(beliefs,messages);
    }
  }

  private class ObservedEdge(val parent: Language, val child: Language) extends Edge {
    def update(state: BeliefState) = {
      case class UpdateBundle(newParentBelief: Lazy[Belief],
                              newC2PMessage: Lazy[Belief]);

      val lazyBundle = for {
        mu_p <- state.lazyBelief(parent);
        mu_c <- state.lazyBelief(child);
        msg2p <- state.lazyMessage(child, parent)
      } yield {
        val mu_p_div_c = mu_p / msg2p
        val lazyNewMu_p = projectMessages(mu_p_div_c, mu_c, edgeFor(parent,child))._1;
        val newMsg2p = for (newMu_p <- lazyNewMu_p) yield newMu_p / mu_p;
        UpdateBundle(lazyNewMu_p, newMsg2p);
      }

      val beliefs = state.beliefs.updated(parent, lazyBundle.flatMap(_.newParentBelief));
      val messages = (state.messages.updated(child -> parent, lazyBundle.flatMap(_.newC2PMessage)));

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
            val myEdges = a.children.map(t.label -> _.label).map(_ -> depth);
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
