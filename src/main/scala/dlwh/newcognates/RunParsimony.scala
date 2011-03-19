package dlwh.newcognates

import scalanlp.config.Configuration
import java.io.File
import scalanlp.util.Index
import scalanlp.fst.Alphabet
import scalanlp.fst.fast.{AutomatonFactory}
import scalala.tensor.sparse.SparseVector
import collection.mutable.ArrayBuffer
import scalala.tensor.dense.DenseVector

/**
 *
 * @author dlwh
 */

object RunParsimony {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.drop(1).map(new File(_)));
    val legalGloss = args(0).split(':').map(Symbol(_)).toSet;
    println(legalGloss);
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    val deathScore = math.log(config.readIn[Double]("initDeathProb"));

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val basetree: Tree = dataset.tree;
    val tree = basetree.subtreeAt(config.readIn[String]("subtree",basetree.label));
    val leaves = tree.leaves;

    val cogs = dataset.cognates.filter(group => legalGloss(group.head.gloss));
    val alphabet = Index(cogs.iterator.flatMap(_.iterator).flatMap(_.word.iterator) ++ Iterator.single(implicitly[Alphabet[Char]].epsilon));

    val gold: IndexedSeq[Seq[Cognate]] = cogs.map(_.filter(cog => leaves(cog.language))).filterNot(_.isEmpty);
    println(gold.length);
    val goldTags = Accuracy.assignGoldTags(gold);
    val numGold = Accuracy.numGold(gold);

    val data = gold.flatten
    println(data.length);
    val randomized = data;
    val legalWords = randomized.toSet;

    import scalanlp.math.Semiring.LogSpace._;
    val autoFactory = new AutomatonFactory(alphabet);
    val factorFactory = new ParsimonyFactorsFactory(autoFactory)
    import factorFactory.EdgeExpectedCounts;
    import factorFactory.factory._;
    val cognatesByGloss = legalWords.groupBy(_.gloss);
    val legalByGloss: Map[Symbol, Set[String]] = legalWords.groupBy(_.gloss).mapValues( cognates => cognates.map(_.word));

    val learningEpochs = config.readIn[Int]("learningEpochs",0);


    type ECounts = Map[(Language,Language),EdgeExpectedCounts];

    def sumCounts(s1: ECounts, s2: ECounts) = {
      val r = collection.mutable.Map[(Language,Language),EdgeExpectedCounts]();
      r ++= s1;
      for( (k,v) <- s2) {
        if(r.contains(k)) r(k) += v
        else r(k) = v;
      }
      r.toMap

    }

    val allEdges = tree.edges;
    var costs = ComputeOracleWeights.costMatrix(index,config);
    var starterProbs = Map.empty[(Language,Language),Double].withDefaultValue(0.9);
    for(iter <- 0 to learningEpochs) {
      def innovationProb(a: Language, b: Language) = starterProbs(a -> b);

      val mapped = legalByGloss.mapValues { legalWords =>
        val factors = new factorFactory.WordFactors(legalWords, costs, innovationProb _, viterbi = true);
        factors
      }


      def decodeCognates(inference: ParsimonyInference[factorFactory.WordFactors],
                         tree: Tree,
                         cur: ArrayBuffer[Cognate]= new ArrayBuffer[Cognate]):IndexedSeq[IndexedSeq[Cognate]] = tree match {
        case t: Child => error("no")
        case Ancestor(label,children) =>
          val childGroups = for(c <- children if inference.hasUpwardMessage(c.label)) yield {
            val edges = inference.edgeMarginal(label,c.label).get;
            val fromChild = if(c.isInstanceOf[Child]) {
              var rest = IndexedSeq.empty[Cognate];
              for( (w,e) <- inference.groupedByLanguage(c.label) zip edges) {
                if(e.posteriorInnovationProb >= 0.5) rest :+= w
                else cur += w
              }
              rest.map(IndexedSeq(_));
            } else if(edges.head.posteriorInnovationProb < 0.5){
              decodeCognates(inference,c,cur).drop(1)
            } else {
              decodeCognates(inference,c);
            }
            fromChild;
          }

          (cur +: (childGroups.flatten.filterNot(_.isEmpty))).toIndexedSeq;
      }

      val groupsAndCounts = (for( (gloss,factors) <- mapped.iterator) yield {
        val inference: ParsimonyInference[factorFactory.WordFactors] = new ParsimonyInference(factors,tree,cognatesByGloss(gloss).toSeq);
        println(gloss + " " + inference.likelihood);
        val string = tree.prettyString{ lang =>
          if(!inference.hasUpwardMessage(lang)) None
          else {
            if(lang == tree.label) Some(lang + ": " + inference.beliefs(lang));
            else Some(lang + ": " + inference.beliefs(lang) + " " + inference.edgeMarginal(inference.parent(lang),lang).get.map(_.posteriorInnovationProb));
          }
        }
        println(string);

        val byLanguage = cognatesByGloss(gloss).groupBy(_.language).mapValues(_.toIndexedSeq);
        val goldTree = GoldStatus.buildTree(tree,byLanguage,goldTags).get._1;
        println(goldTree.prettyString(Some(_)));

        val myGroups = decodeCognates(inference,tree).filterNot(_.isEmpty);
        println(myGroups);
        assert(myGroups.iterator.map(_.size).sum == cognatesByGloss(gloss).size);


        val flattenedGroups = myGroups.map(a => CognateGroup(a:_*));
        // expectedCounts
        val edgeCounts = (for (pair@(parent, child) <- allEdges.iterator;
                               marginal <- inference.edgeMarginal(parent, child).iterator;
                               edge <- marginal.iterator)
        yield {pair -> edge.expectedCounts}).toMap;

        (flattenedGroups,edgeCounts);
      }).toIndexedSeq;


      val guessGroups = groupsAndCounts.iterator.map(_._1).reduceLeft(_ ++_);
      val counts = groupsAndCounts.iterator.map(_._2).reduceLeft(sumCounts);

      val newStarterProbs: Map[(Language, Language), Double] = for( (edge,ecounts) <- counts) yield {
        val r = edge -> ((ecounts.probInnovation + 0.5 * 20) /(ecounts.n + 20));
        println("XXX",r);
        r
      }

      costs = EditDistanceObjectiveFunction.optimize(index,counts.mapValues(_.alignmentCounts));
//      if(iter > 2)
//        starterProbs = newStarterProbs

      val (precision,recall) = Accuracy.precisionAndRecall(goldTags, numGold, guessGroups)
      val f1 = 2 * (precision * recall) / (precision + recall)
      println(":: " + iter + "\t"+ precision + "\t" + recall +"\t" + f1 + "\t" + guessGroups.length + "\t" + Accuracy.purity(goldTags,guessGroups));
    }

  }



}