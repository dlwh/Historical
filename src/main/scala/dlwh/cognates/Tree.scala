package dlwh.cognates;

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

import scala.io.Source;

sealed trait Tree { val label: String};
case class Ancestor(label: String, lchild: Tree, rchild: Tree) extends Tree;
case class Child(label: String) extends Tree;

object Tree {
  def readTree(s: String) = {
    object parser extends StdLexical with ImplicitConversions with Scanners {
      private val ws = whitespace;
      private val other = acceptIf( c => !c.isWhitespace && c != '(' && c != ')')( "'" + _ + "'not expected");
      private val rparen = (ws ~> accept(')')) <~ ws;
      private val lparen = (ws ~> accept('(')) <~ ws;
      private val tok = rep(other) ^^ { x => x.mkString("")};

      def tree: Parser[Tree] = (
        lparen ~> tok <~ rparen ^^ { Child(_)  }
        | lparen ~> (tok ~ tree ~ (ws ~> tree <~ rparen )) ^^ { 
          case  tok~tree~right => Ancestor(tok,tree,right)
        }
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


}
