package dlwh.newcognates

import dlwh.cognates.{NormalizedTransitions, PosUniCompression}
import scalanlp.fst.{DecayAutomaton}

/**
 * 
 * @author dlwh
 */

object ToyDataset  {
  val cognates = Seq(Cognate("fuoco","a",'Fire), Cognate("fuego","b",'Fire), Cognate("f","b",'Fire), Cognate("ff","b",'Fire));
  val ca = cognates(0);
  val cb = cognates(1);
  val tree = new Ancestor("Anc",Seq(new Ancestor("aa",Seq(new Child("a"))),new Ancestor("bb",Seq(new Child("b")))))
  val alphabet = cognates.iterator.flatMap(_.word.iterator).toSet

  val beginningUnigram = '#';
  val compressor = new PosUniCompression(14,beginningUnigram) with NormalizedTransitions[Int,Char] : MessageCompressor[_];
  def editDistance(l: Language, l2: Language)= new scalanlp.fst.EditDistance(-0.3,-0.4,alphabet);
  def initBelief(l: Language) = new DecayAutomaton(5, alphabet);
  def initMessage(a: Language, b: Language) = new DecayAutomaton(40, alphabet);
  val factors = new TransducerFactors(alphabet, compressor, initBelief(""), editDistance _,  initMessage _);
}