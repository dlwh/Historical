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

  private val hasMessage = computeMessageHavers(tree);

  val children = {
    def buildChildren(tree: Tree, parent: Language):Map[Language,Set[Language]] = tree match {
      case _ : Child => Map(tree.label -> Set.empty[Language]);
      case a: Ancestor =>
        val myContribution = (a.label -> (a.children.map(_.label).toSet));
        a.children.map(buildChildren(_,a.label)).reduceLeft(_ ++ _) + myContribution;
    }
    buildChildren(tree,"ROOT");
  }

  val initialBeliefs: BeliefState = {
    val messages = Map.empty ++ (for {
      (l,ns) <- children iterator;
      n <- (ns + parent(l)) iterator
    } yield {
      val msg = if (n == "ROOT") rootMessage else initialMessage(n, l)
      (n->l) -> msg
    });
    val beliefs = group.cognates.mapValues(c => beliefForWord(c.word));

    val defaultedBeliefs = beliefs.toMap.withDefault{ l =>
      val ms = for( n <- children(l).iterator if hasMessage(n)) yield messages(n -> l);
      (ms.foldLeft(messages(parent(l) -> l))(_ * _ ) * factors.initialBelief(l) ).normalized :Belief
    }
    new BeliefState(defaultedBeliefs,messages);
  }

  lazy val onePassBeliefs = initialBeliefs.upward;

  // TODO: make upward better.
  def beliefsAt(l: Language) = onePassBeliefs.updateToward(l).belief(l);

  class BeliefState( val beliefs: Map[Language,Belief],
                     val messages: Map[(Language,Language),Belief]) {
    lazy val likelihood = {
      println(beliefs.mapValues(_.partition));
      beliefs.valuesIterator.map(_.partition).sum;
    }

    def message(from: Language, to: Language): Belief = messages(from -> to);

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
        edge.updateChild(state);
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
      println(child + " up " + parent);
      val (mu_p_div_c,mu_c_div_p,edgeMarginal) = marginals(state);
      val newMu_p = edgeMarginal.parentProjection;
      val newMsg2p = (newMu_p / mu_p_div_c)
      val beliefs = state.beliefs.updated(parent,newMu_p);
      val messages = state.messages.updated(child -> parent, newMsg2p);
      new BeliefState(beliefs,messages);
    }

    def isChildObserved = group.cognates.contains(child);

    def updateChild(state: BeliefState) = if(isChildObserved) {
      state
    } else {
      println(parent + " down " + child);
      val (_,mu_c_div_p,edgeMarginal) = marginals(state);
      val newMu_c = edgeMarginal.childProjection;
      val newMsg2c = (newMu_c / mu_c_div_p)

      val beliefs = state.beliefs.updated(child,newMu_c);
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
      val mu_p_div_c = mu_p / msg2p normalized;
      val mu_c_div_p = try { mu_c / msg2c normalized } catch {
        case e =>
          println(parent,child);
          println(mu_c,mu_c.partition);
          println(msg2c);
          println(mu_c / msg2c);
        throw new RuntimeException("ffff",e);
      };
      (mu_p_div_c,mu_c_div_p,edgeFor(parent,child).edgeMarginal(mu_p_div_c, mu_c_div_p));
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


}
