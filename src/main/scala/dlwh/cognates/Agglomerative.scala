package dlwh.cognates

import scalanlp.concurrent.ParallelOps._;
import scala.collection.mutable.PriorityQueue;
import Bipartite._
import java.util.Properties
import scalanlp.config._;
import scalala.Scalala._;
import java.io.FileInputStream
import scalanlp.stats.sampling._;
import java.io.File

import Types._;

class Agglomerative[F<:Factors](val tree: Tree,
                                languages: Seq[Language],
                                treePenalty: Double,
                                initDeathProb: Double,
                                factorBroker: FactorBroker[F]) {

  def iterations(words: IndexedSeq[Cognate], groupingsPerStep: Int = 100):Iterator[State[F]] = {
    Iterator.iterate(initialState(words))(step(_,groupingsPerStep));
  }

  def initialState(words: IndexedSeq[Cognate]) = {
    val groups = words.map(c => Map(c.language -> c));
    State(groups, factorBroker.initialFactors, Map.empty.withDefaultValue(initDeathProb));
  }
  
  def step(state: State[F], numGroupings: Int = 100) = {
    val ios = makeIOs(state);
    val affinities = computeInitialAffinities(ios);
    
    var ll = ios.foldLeft(0.0)(_+_.likelihood);
    println("LL comparison: ",ll,state.likelihood);

    var grouped = 0;
    var groups = ios.toSet;
    while(!affinities.isEmpty && grouped < numGroupings) {
      val (old1, old2, newg, score) = affinities.dequeue();
      if(groups.contains(old1) && groups.contains(old2)) {
        groups -= old1;
        groups -= old2;
        ll += score;
        println("Merge " + old1.words.values.mkString(",") + " " + old2.words.values.mkString(",") + " " + score);

        grouped += 1;
        val newMerges = for{
          g2 <- groups.toIndexedSeq.par;
          (merged,score) <- mergeScore(newg,g2)
        } yield (newg,g2,merged,score);
        groups += newg;
        
        affinities ++= newMerges;
      }
    }
    println("Iter finished. LL: " + ll);
    state.copy(groups.iterator.map(_.words).toIndexedSeq, likelihood = ll);
  }

  private def makeIOs(s: State[F]) = {
    val ios: IndexedSeq[InsideOutside[F]] = s.groups.par.map{g =>
      val io = s.makeIO(tree,g);
      println(g);
      io.likelihood; // precompute likelihood
      io
    };
    ios;
  }

  def computeInitialAffinities(ios: IndexedSeq[InsideOutside[F]]) = {
    val scores: IndexedSeq[(InsideOutside[F],InsideOutside[F], InsideOutside[F], Double)] = for {
      g1Index <- ios.zipWithIndex.par;
      g2 <- ios.view(g1Index._2 + 1,ios.length)
      (merged,score) <- mergeScore(g1Index._1,g2)
    } yield (g1Index._1,g2,merged,score);

    val pq = new PriorityQueue[(InsideOutside[F],InsideOutside[F], InsideOutside[F], Double)]()(Ordering[Double].on(_._4));
    pq ++= scores
    pq
  }

  // Computes the loss or gain for merging two groups.
  private def mergeScore(gi: InsideOutside[F], gj: InsideOutside[F]) = {
    if(gi.words.head._2.gloss != gj.words.head._2.gloss) None
    else for(merged <- gi merge gj)
      yield (merged, merged.likelihood - gi.likelihood - gj.likelihood);
  }

}

object AgglomerativeRunner {
  import Accuracy._;

  def main(args: Array[String]) {
    val props = new Properties();
    props.load(new FileInputStream(new File(args(0))));
    val config = Configuration.fromProperties(props);
    val languages = config.readIn[String]("dataset.languages").split(",");

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val gold = dataset.cognates.map(_.filter(cog => leaves(cog.language)));
    val goldIndices = indexGold(gold);

    val data = gold.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);

    val Params(treePenalty,initDeathProb,allowDeath) = config.readIn[Params]("params");
    val messageCompression = readAutomataCompressor(config, "transducers.message");

    val transducerCompression = config.readIn[String]("transducers.editdistance","uni") match {
      case "unigram" | "uni" =>
        new UniCompression(('#','#')) with NormalizedByFirstChar[Unit,Char];
    }

    //val rootCompression = readAutomataCompressor(config, "transducers.root");

    val shouldLearn = config.readIn[Boolean]("transducers.learning");
    val initialFactors = new TransducerFactors(tree,alphabet,messageCompression);
    val broker = (
      if(!shouldLearn) new ConstantBroker(initialFactors)
      else new TransducerBroker(tree, alphabet, initialFactors, messageCompression, transducerCompression)
    );


    val bipartite = new Agglomerative[TransducerFactors](tree, languages, treePenalty, initDeathProb, broker);
    val iter = bipartite.iterations(randomized);
    val numPositives = numberOfPositives(languages, gold);

    for( state <- iter.take(1000)) {
      val numGroups = state.groups.length;
      for(g <- 0 until numGroups) {
        val cognates = state.groups(g);
        val indices = cognates.valuesIterator map goldIndices toSeq;
        println(cognates.mkString(",") + " " + indices.mkString(","));
      }
      println("Conditional Likelihood" + state.likelihood)
      val groundedGroups = state.groups map { group => group.mapValues(goldIndices).toMap };
      val (precisions,recalls) = precisionAndRecall(languages,groundedGroups,numPositives);
      println("Precisions" + precisions);
      println("Recall" + recalls);
    }
  }

  case class MessageParams(`type`: String, klThreshold: Double, maxStates: Int);

  private def readAutomataCompressor(config: Configuration, prefix: String): MessageCompressor[_] = {
    val MessageParams(tpe, klThreshold, maxStates) = config.readIn[MessageParams](prefix);
    val beginningUnigram = '#';
    tpe match {
      case "bigram" | "bi" =>
        new BiCompression(klThreshold,maxStates,beginningUnigram) with NormalizedTransitions[Option[Char],Char] : MessageCompressor[_];
      case "unigram" | "uni" =>
        new UniCompression(beginningUnigram) with NormalizedTransitions[Unit,Char] : MessageCompressor[_];
      case "posunigram" | "posuni" =>
        new PosUniCompression(maxStates,beginningUnigram) with NormalizedTransitions[Int,Char] : MessageCompressor[_];
    }
  }
}