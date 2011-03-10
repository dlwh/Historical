package dlwh.newcognates

import scalanlp.concurrent.ParallelOps._
import scalanlp.fst.fast.AutomatonFactory
import scalanlp.config.Configuration
import scalanlp.fst.Alphabet
import scalanlp.util.Index
import java.util.zip.GZIPOutputStream
import java.io._
import dlwh.cognates.{UniCompression, NormalizedByFirstChar, Compressor}
;

/**
 * 
 * @author dlwh
 */

object AlignAndDump  {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val cogs = dataset.cognates.map(_.filter(c => leaves(c.language)));
    println(dataset.cognates.iterator.flatMap(_.iterator.map(_.word.length)).max);
    println(cogs.length);


    import scalanlp.math.Semiring.LogSpace._;
    val transducerCompressor: Compressor[_,(Char,Char)] = new UniCompression[(Char,Char)]('#' -> '#') with NormalizedByFirstChar[Unit,Char];

    val dir = new File("alignments")
    dir.mkdirs();

    cogs.zipWithIndex.par.foreach { case (cogs,i) =>
      val alphabet = Set.empty ++ cogs.iterator.flatMap(_.word.iterator)
      println(alphabet);
      def eps = implicitly[Alphabet[Char]].epsilon;
      val allPairs = for {
        a <- alphabet + eps;
        b <- alphabet + eps;
        if a != eps || b != eps
      } yield (a,b);


      val autoFactory = new AutomatonFactory(Index(alphabet + eps));
      val factorFactory = new FastTransducerFactory {
        val factory = autoFactory;
        val model = new factory.PositionalUnigramModel(13);
      }
      import factorFactory.factory._;

      def editDistance(l: Language, l2: Language)= new EditDistance(-4,-5);
      def initBelief(l: Language) = new MaxLengthAutomaton(11);//new DecayAutomaton(1);
      def initMessage(a: Language, b: Language) = new UnigramModel;
      val rootBelief = new DecayAutomaton(4);
      val factors = new factorFactory.FastTransducerFactors(rootBelief, initBelief _, editDistance _,  initMessage _);

      val group = CognateGroup(cogs: _*)
      println(i,group.prettyString(tree));
      val ti = new TreeInference(factors,tree,CognateGroup(cogs:_*));
      val beliefs = ti.initialBeliefs.upward.downward;
      val marginals = (for(edge@(parent,child) <- tree.edges.iterator; marginal <- beliefs.edgeMarginal(parent,child).iterator) yield {
        println(edge);
        val trans = asNormalTransducer(marginal.trans).relabel
        val cost = transducerCompressor.gatherStatistics(allPairs,trans);
        (edge) -> cost._1;
      });
      println(i,beliefs.likelihood);
      val out = new ObjectOutputStream(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(new File(dir,i +".ser.gz")))));
      out.writeObject(marginals.toIndexedSeq);
      out.close();
    }
  }
}