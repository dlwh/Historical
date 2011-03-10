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

  private val hasMessage = group.nodesWithObservedDescendants(tree);

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
      val r = (ms.foldLeft(messages(parent(l) -> l))(_ * _ ) * factors.initialBelief(l) ).normalized :Belief
      r
    }
    new BeliefState(defaultedBeliefs,messages);
  }

  lazy val onePassBeliefs = initialBeliefs.upward;

  // TODO: make upward better.
  def beliefsAt(l: Language) = onePassBeliefs.updateToward(l).belief(l);

  class BeliefState( val beliefs: Map[Language,Belief],
                     val messages: Map[(Language,Language),Belief]) {
    lazy val likelihood = {
      val str2 = tree.prettyString( lang =>
        if(hasMessage(lang))  beliefs.get(lang).map(belief => lang + " marginal: " + group.cognates.mapValues(c => c.word + " " +belief(c.word)))
        else None
      )
      println(str2);
      val grouped = messages.groupBy(_._1._2);
      val integrated = (beliefs.keys).map{l => (l -> grouped(l).valuesIterator.reduceLeft(_*_).partition)} toMap;
      val str3 = tree.prettyString( lang =>
        if(hasMessage(lang))  beliefs.get(lang).map(belief => lang + "correction" + integrated(lang));
        else None
      )
      println(str3);
      integrated.filterNot(ROOT ==  _._1).iterator.map(_._2).sum
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
      val r = updateOrder.foldLeft(this:BeliefState) { (state,languagePair) =>
        val (parent,child) = languagePair;
        val edge = new Edge(parent,child);
        edge.updateParent(state);
      }
      r
    }

    def downward: BeliefState = {
      val r = updateOrder.foldRight(this:BeliefState) { (languagePair, state) =>
        val (parent,child) = languagePair;
        val edge = new Edge(parent,child);
        edge.updateChild(state);
      }
      r.likelihood
      r
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
      update(state);
    }

    def updateChild(state: BeliefState) = update(state);

    def update(state: BeliefState) = {
      println(parent,child);
      val (mu_p_div_c,mu_c_div_p,edgeMarginal) = marginals(state);
      lazy val newMu_p = edgeMarginal.parentProjection; // Z_i * q(x_parent)
      lazy val newMsg2p = (newMu_p / mu_p_div_c);   // Z_i * q(x_parent) / q^\i(x_parent)
      lazy val newMu_c = edgeMarginal.childProjection; // Z_i * q(x_child)
      lazy val newMsg2c = (newMu_c / mu_c_div_p);// Z_i * q(x_child) / q^\i(x_child)
      var beliefs = state.beliefs;
      var messages = state.messages;
      if(parent != "ROOT") {
        beliefs = beliefs.updated(parent,newMu_p)
      };
      if(!isChildObserved) {
        beliefs = beliefs.updated(child,newMu_c)
      };
      messages = messages.updated(parent->child,newMsg2c);
      messages = messages.updated(child -> parent, newMsg2p.scaleBy(-newMu_p.partition))
      new BeliefState(beliefs,messages);
    }
    def isChildObserved = group.cognates.contains(child);
    /*
    def updateParent(state: BeliefState) = {
      println(child + " up " + parent);
      val (mu_p_div_c,mu_c_div_p,edgeMarginal) = marginals(state);
      val newMu_p = edgeMarginal.parentProjection;
      val newMsg2p = (newMu_p / mu_p_div_c);
      val beliefs = state.beliefs.updated(parent,newMu_p);
      val messages = state.messages.updated(child -> parent, newMsg2p);
      new BeliefState(beliefs,messages);
    }


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
    */

    def edgeMarginal(state:BeliefState) = {
      marginals(state)._3;
    }

    private def marginals(state:BeliefState) = {
      val mu_p = state.belief(parent);
      val mu_c = state.belief(child);
      lazy val msg2p = state.message(child, parent);
      val msg2c = state.message(parent, child)
      val mu_p_div_c = if(parent != ROOT) (mu_p / msg2p) normalized else mu_p;
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
    val forward = inferDepth(tree,0).toSeq.sortBy(-_._2).map(_._1) :+ (ROOT -> tree.label)
    forward
  }
  println(updateOrder);


}
