package dlwh.baldur

import scala.io.Source
import java.io.{FileInputStream, BufferedInputStream, File}
import phylo.Tree

class ResourceDataset(name: String, val languages: Seq[String]=Seq.empty, hasGloss: Boolean = false) extends Dataset {
  def cognates = Cognates.readCognates( this.getClass.getClassLoader().getResourceAsStream(name + "_ipa"), languages, hasGloss);
  def tree =  {
    val stream = this.getClass.getClassLoader().getResourceAsStream(name + "_tree");
    // read until we get to a blank line.
    val line = Source.fromInputStream(stream).mkString;
    stream.close();
    Tree.parseLisp(line)
  }

  def base = this;

}

class FileDataset(ipaPath: File, treeName: String, val languages: Seq[Language], hasGloss: Boolean=false) extends Dataset {
  def cognates = Cognates.readCognates( new BufferedInputStream(new FileInputStream(ipaPath)), languages, hasGloss);
  def tree =  {
    val stream = this.getClass.getClassLoader().getResourceAsStream(treeName + "_tree");
    // read until we get to a blank line.
    val line = Source.fromInputStream(stream).mkString
    stream.close();
    Tree.parseLisp(line)
  }

  def base = this;

}
