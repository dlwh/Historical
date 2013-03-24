package dlwh.cognates

import util.parsing.combinator.RegexParsers
import java.io.Reader
import pal.tree.{Node, SimpleNode, Tree => palTree}
import collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import collection.BitSet
import util.control.Breaks._

/**
 * 
 * @author dlwh
 */
final case class Tree[@specialized(Int) +L](
  label: L,
  branchLength: Double=0.0,
  time: Double=0.0,
  children: IndexedSeq[Tree[L]] = IndexedSeq.empty) {


  override def toString = recToString(0)

  def isLeaf = children.isEmpty;


  def maxLengthToLeaf:Double = {
    if(isLeaf) 0.0
    else {
      children.map(c => c.maxLengthToLeaf + c.branchLength).max;
    }
  }

  def minLengthToLeaf:Double = {
    if(isLeaf) branchLength
    else {
      branchLength + children.map(c => c.maxLengthToLeaf + c.branchLength).min;
    }
  }

  // fill in times using branch lengths
  def treeWithTimes( time: Double): Tree[L] = {
    new Tree( label, branchLength, time, children.map(
      c => c.treeWithTimes( time - c.branchLength)))
  }

  private def recToString(depth: Int): String = {
    children.foldLeft( "Tree(" + label +", " + branchLength + ")"){
      (acc,child) => acc + "\n  " + "  "*depth + child.recToString(depth+1)
    }
  }

  def map[U](f: L=>U):Tree[U] = copy(label=f(label),children=children.map(_.map(f)));

  def toPAL:palTree = {
    new pal.tree.SimpleTree(toPALNode)
  }

  private def toPALNode:pal.tree.Node = {
    if(isLeaf)
      new SimpleNode(label.toString,branchLength)
    else new Tree.ChildNode(children.map(_.toPALNode).toArray, branchLength);
  }

  def findSubtree(f: Tree[L]=>Boolean):Option[Tree[L]] = if(f(this)) Some(this) else {
    children.iterator.flatMap(_.findSubtree(f)).toStream.headOption
  }

  def subtreeAt(l: String) = findSubtree(_.label == l).get;

  def leaves:Seq[L] = if(isLeaf) Seq(this.label) else children.flatMap(_.leaves).toSeq
  def edges:Seq[(L,L)] = if(isLeaf) Seq.empty else children.flatMap(_.edges).toSeq ++ children.map( label -> _.label).toSeq

  def prettyString(f: L=>Option[String], sb: StringBuilder = new StringBuilder, depth: Int = 0):StringBuilder = {
    for(out <- f(label)) {
      sb ++= "\t"*depth + "(";
      sb ++= out.replaceAll("\n","\t"*depth + "\n")
      sb ++= "\n";
      for(c <- children) {
        c.prettyString(f,sb,depth+ 1);
      }
      sb ++= "\t"*depth + ")\n";
    }
    sb
  }


  /**
   * Doesn't include final ';'
   */
  def toNewickString:String = {
    val branchSet = if(children.isEmpty) "" else children.map(_.toNewickString).mkString("(",", ",")");
    branchSet + label + {if(branchLength != 0.0) ":" + branchLength else ""}
  }

  def extend[U](f: Tree[L]=>U):Tree[U] = {
    Tree( f(this), branchLength, time, children.map(_ extend f))
  }

  /**
   * Labels each tree with a unique integer, with the root having the highest number
   * Returns the label at each index
   */
  def labelPostOrder: (Tree[Int],IndexedSeq[L]) = {
    val buf = ArrayBuffer[L]()
    def rec(tree: Tree[L], buf: ArrayBuffer[L]):Tree[Int] = {
      val newChildren = tree.children.map(rec(_,buf))
      buf += tree.label
      Tree( buf.size - 1, tree.branchLength, time, newChildren)
    }

    rec(this,buf) -> buf
  }

  def postorder:Iterator[Tree[L]] = {
    children.iterator.flatMap(_.postorder) ++ Iterator(this)
  }

  // Returns partitions induced by the tree, respecting the root,
  // in postfix order.
  // Requires that leaves are labeled serially and L = Int.
  def partitions = {
    // recursion with side-effect: all return values are accumulated.
    val acc = ArrayBuffer.empty[BitSet]
    def p( t: Tree[Int]) : BitSet = {
      var r = if( t.isLeaf) BitSet( t.label) else
        (BitSet.empty /: t.children.map( p( _)))( _ ++ _)
      acc += r
      r
    }
    p( this.asInstanceOf[Tree[Int]]) // not safe, but a pain to fix
    acc.toIndexedSeq
  }

  def treeReduce[A]( f: (Tree[L], Seq[A]) => A): A = {
    f( this, children.map( _.treeReduce( f)))
  }

  def distanceToLeaves[U>:L]:Map[U, Double] = {
    def rec(t: Tree[L], distSoFar: Double):Map[U,Double] = {
      if(t.isLeaf) Map(t.label -> (distSoFar + t.branchLength))
      else {
        t.children.foldLeft(Map.empty[U,Double])((m,t2) => m ++ rec(t2, distSoFar + t.branchLength))
      }
    }
    rec(this, 0.0)
  }
}

// Mutable Tree, convenient for building trees.
final case class MutTree[@specialized(Int) L](
  var label: L,
  var time: Double=0.0,
  val children: ArrayBuffer[MutTree[L]] = ArrayBuffer.empty[MutTree[L]])
{
  def isLeaf = children.isEmpty;

  def toTree( implicit pTime: Double = Double.PositiveInfinity) : Tree[L] =
    Tree( label, pTime-time, time, children.map( _.toTree( time)))
}

object Tree {

  private class ChildNode(tree: Array[Node], branchLength: Double) extends SimpleNode(tree, branchLength)

  object NewickParser extends RegexParsers {
    lazy val tree:Parser[Tree[String]] = branch <~ opt(';') // extension: I don't care about the ;.
    lazy val subtree:Parser[Tree[String]] = (internal|leaf)
    lazy val leaf = name                                   ^^ { Tree(_) }
    lazy val internal = ("(" ~>  branchSet <~  ")") ~ name ^^ {case bs~name => Tree(name,0.0,0.0,bs.toIndexedSeq)}
    lazy val branchSet:Parser[Seq[Tree[String]]] = repsep (branch, ',');
    lazy val branch = (subtree ~ opt(length))              ^^ { case t~l => t.copy(branchLength=l.getOrElse(0.0))};
    lazy val name = regex("[^,;)(:]*".r)
    lazy val length = ":" ~> meta ~> "[0-9\\.]+".r ^^ { _.toDouble } | ":Infinity" ^^ ( _ => Double.PositiveInfinity) | "" ^^ ( _ => 0.0)
    lazy val meta = "(\\[.*?\\])?".r
  }

  def parseNewick(str: String):Tree[String] = NewickParser.parseAll(NewickParser.tree,str).get;
  def parseNewick(str: Reader):Tree[String] = NewickParser.parseAll(NewickParser.tree,str).get;


  // A similar format to lisp that Alex Bouchard uses (and I extended to add lengths, etc).
  object LispParser extends RegexParsers {
    lazy val tree:Parser[Tree[String]] = "(" ~> branch <~ ")" <~ opt(';') // extension: I don't care about the ;.
    lazy val branch = name ~ length ~ rep(tree)  ^^ { case name~length~children => Tree(name,length,0.0,children.toIndexedSeq) }
    lazy val name = regex("[^,;)(: ]*".r)
    lazy val length = ":" ~> "[0-9\\.]+".r       ^^ { _.toDouble } | "" ^^ ( _ => 0.0)

  }

  def parseLisp(str: String):Tree[String] = LispParser.parseAll(LispParser.tree,str).get;
  def parseLisp(str: Reader):Tree[String] = LispParser.parseAll(LispParser.tree,str).get;


  // Builds a plurality consensus tree.
  // Partitions must be sorted by count, descending.
  def consensus( partitions: Seq[BitSet],
    times: (BitSet => Double) = (_ => 0.0)):
    (Tree[BitSet], Set[BitSet]) =
  {
    val root_ = ArrayBuffer.empty[MutTree[BitSet]]
    for( p <- partitions) {
      val x_ = {  // where to insert new node
        var x_ = root_
        breakable{ while( true) {
          val i = x_.indexWhere( t =>
            (t.label & p) == p)  // p is subordinate
          if( i == -1) break
          x_ = x_( i).children
        }}
        x_
      }
      if( x_.forall( t =>
        (t.label & p).isEmpty ||    // p is coordinate or
        (t.label & p) == t.label))  // p is superordinate
      {
        val group_ = x_.groupBy( t => (t.label & p).isEmpty)
        x_.clear
        if( group_.contains( true)) x_ ++= group_( true)
        x_ += MutTree( p, times( p),
          if( group_.contains( false)) group_( false)
          else ArrayBuffer.empty[MutTree[BitSet]]
        )
      } // else p is not compatible with consensus
    }

    (root_(0).toTree, root_(0).toTree.postorder.map(_.label).toSet)
  }
}
