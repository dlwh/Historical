package dlwh.newcognates

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

import scala.io.Source;

sealed trait Tree { 
  val label: String
  def map(f: String=>String): Tree
  /**
  * Returns all languages (including l) that are on a path from the root to l
  */
  def predecessorsOfLanguage(l: String): Set[String];
  def edges: Set[(String,String)];
  def leaves: Set[(String)];
};

case class Ancestor(label: String, children: Seq[Tree]) extends Tree {
  def map(f: String=>String) = Ancestor(f(label), children.map(_ map f));
  def predecessorsOfLanguage(l: String) = {
    val path = children.iterator.map(_.predecessorsOfLanguage(l)).reduceLeft( (a,b) => if(a.isEmpty) b else a );
    path + label;
  }
  def leaves = children.flatMap(_.leaves).toSet;
  def edges = children.flatMap(_.edges).toSet ++ children.map( label -> _.label).toSet
}

case class Child(label: String) extends Tree { 
  def map(f: String=>String) = Child(f(label));
  def predecessorsOfLanguage(l : String) = if(l == label) Set(l) else Set.empty;
  def edges = Set.empty;
  def leaves = Set(this.label);
}

object Tree {
  def readTree(s: String) = {
    object parser extends StdLexical with ImplicitConversions with Scanners {
      private val ws = whitespace;
      private val other = acceptIf( c => !c.isWhitespace && c != '(' && c != ')')( "'" + _ + "'not expected");
      private val rparen = (ws ~> accept(')')) <~ ws;
      private val lparen = (ws ~> accept('(')) <~ ws;
      private val tok = rep(other) ^^ { x => x.mkString("")};

      def tree: Parser[Tree] = (
        lparen ~> tok ~ rep(tree) <~ rparen ^^ { case label ~ children => if(children.isEmpty) Child(label) else Ancestor(label,children.toIndexedSeq)  }
      )

      def readTree(input: String) =  {
        phrase(tree)(new CharSequenceReader(input)) match {
          case Success(result,_) => result
          case x => throw new Exception("unexpected" + x);
        }
      }
    }

    parser.readTree(s);
  }

  def romance = {
    val stream = this.getClass.getClassLoader().getResourceAsStream("romance_tree");
    // read until we get to a blank line.
    val line = Source.fromInputStream(stream).getLines().next;
    stream.close();
    readTree(line);
  }

  def basic = {
    val stream = this.getClass.getClassLoader().getResourceAsStream("basic_tree");
    // read until we get to a blank line.
    val line = Source.fromInputStream(stream).getLines().next;
    stream.close();
    readTree(line);
  }

}
