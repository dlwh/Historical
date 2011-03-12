package dlwh.newcognates

/**
 * 
 * @author dlwh
 */

class TreeElimination[F<:Factors](val factors: F, val tree: Tree, val group: CognateGroup) {
  import factors._;
  def parent(l: Language) = parentMap(l);
  private val parentMap = buildParentMap(tree, ROOT)

  private def  buildParentMap(tree: Tree, parent: Language):Map[Language,Language] = tree match {
    case _ : Child => Map(tree.label -> parent);
    case a: Ancestor => a.children.map(buildParentMap(_,a.label)).reduceLeft(_ ++ _) + (a.label -> parent);
  }

  private val hasUpwardMessage = group.nodesWithObservedDescendants(tree);

  val children = {
    def buildChildren(tree: Tree, parent: Language):Map[Language,Set[Language]] = tree match {
      case _ : Child => Map(tree.label -> Set.empty[Language]);
      case a: Ancestor =>
        val myContribution = (a.label -> (a.children.map(_.label).toSet));
        a.children.map(buildChildren(_,a.label)).reduceLeft(_ ++ _) + myContribution;
    }
    buildChildren(tree,"ROOT");
  }

  val upwardMessages:collection.mutable.Map[Language,Belief] = new collection.mutable.HashMap[Language,Belief] {
    override def default(l: Language) = {
      val b:Belief = upwardBeliefs(l);
      val message = edgeFor(parent(l),l).edgeMarginal(initialBelief(parent(l)),b).parentProjection;
      update(l,message);
      message
    }
  }

  val upwardBeliefs: collection.mutable.Map[Language,Belief] = new collection.mutable.HashMap[Language,Belief] {
    override def default(l: Language) = {
      val belief =  if(group.cognates.contains(l)) {
        beliefForWord(group.cognates(l).word)
      } else  {
        val messages = for(c <- children(l) if hasUpwardMessage(c)) yield upwardMessages(c);
        messages.reduceLeft(_ * _);
      }
      update(l,belief);
      belief;
    }

  }

  val edgeMarginals:collection.mutable.Map[(Language,Language),Edge] = new collection.mutable.HashMap[(Language,Language),Edge] {
    override def default(pair: (Language,Language)) = {
      val (p,l) = pair;
      val messageToP = if(p == tree.label) rootMessage else edgeMarginals(parent(p),p).childProjection;
      val messages = for(c <- children(p) if hasUpwardMessage(c) && l != c) yield upwardMessages(c);
      val qp = messages.foldLeft(messageToP)(_ * _);
      val message = edgeFor(parent(l),l).edgeMarginal(qp,upwardBeliefs(l));
      update(p->l,message);
      message
    }
  }

  def likelihood  = upwardBeliefs(tree.label).partition;

  def edgeMarginal(parent: Language, child: Language) = if(hasUpwardMessage(child)) Some(edgeMarginals(parent->child)) else None;
}

