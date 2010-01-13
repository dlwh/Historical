package dlwh.cognates
import scalanlp.fst._;
import dlwh.cognates.Types._;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;

/*
class EditDistanceLearner(tree: Tree, numStates: Int, alphabet: Set[Char]) {
  val edgesToLearn = tree.edges.toSeq;
  import TrigramSemiring._;
  val tc = new TriCompression[(Char,Char)](1.0, 5,alphabet.map { a => (a,a)}, Set.empty, ('#','#') );

  def learnStates(ios: Iterator[InsideOutside[TransducerFactors]]) = {
    val trigrams = LogPairedDoubleCounter[Bigram[(Char,Char)],(Char,Char)]();
    val transducers = collection.mutable.Map[(Language,Language),Transducer[Double,_,Char,Char]]();
    for( io <- ios) {
      for( (fromL,toL) <- edgesToLearn) yield {
        val ctr = LogPairedDoubleCounter[(Char,Char),(Char,Char)]();
        val ring = new TrigramSemiring[(Char,Char)]( alphabet.map { a => (a,a)}, Set.empty, ('#','#') );
        import ring._;
        val trans = io.edgeMarginal(fromL, toL);
        val cost = trans.fst.reweight(promote _, promoteOnlyWeight _ ).cost;
        for( (k1,k2,v) <- cost.decodeBigrams.triples) {
          ctr(k1,k2) = logSum(ctr(k1,k2),v);
        }
      }
    }

    transducers += ((fromL,toL) -> tc.compress(0.0,trigrams,ctr));

    Map.empty ++ transducers;
  }
}

*/