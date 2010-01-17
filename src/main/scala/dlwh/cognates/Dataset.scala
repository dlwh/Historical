package dlwh.cognates

import scala.io.Source;

class Dataset(name: String, languages: Seq[String]) {
  def cognates = Cognates.readCognates(name + "_ipa", languages);
  def tree =  {
    val stream = this.getClass.getClassLoader().getResourceAsStream(name + "_tree");
    // read until we get to a blank line.
    val line = Source.fromInputStream(stream).getLines().next;
    stream.close();
    Tree.readTree(line)
  }
}
