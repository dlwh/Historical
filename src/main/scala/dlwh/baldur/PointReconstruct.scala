package dlwh.baldur

import scalanlp.config.Configuration
import java.io.File
import collection.{Seq, IndexedSeq}
import scalanlp.util.Index
import math._
import scalanlp.stats.distributions.{Poisson, Rand, Multinomial}
import collection.mutable.ArrayBuffer
import scalala.library.Numerics._
import scalanlp.math.Semiring.LogSpace._
import scalanlp.newfst.{Automaton, EditDistance}
import dlwh.editdistance.GeneralEditDistance
import scalala.tensor.dense.{DenseVector, DenseMatrix}

/**
 * 
 * @author dlwh
 */
object PointReconstruct extends App {
  val configuration = Configuration.fromPropertiesFiles(args.map(new File(_)))

  val dataset = Dataset.fromConfiguration(configuration)

  val originLang = dataset.tree.label
  val targetLang = configuration.readIn[String]("modernLang")

  val myLangs = Set(originLang,targetLang)
  println(myLangs)

  val words: IndexedSeq[Seq[Cognate]] = dataset.cognates.map(_.filter(cog => myLangs(cog.language))).filter(_.nonEmpty)

  val pairs = for {
    set <- words
    top = set.find(_.language == originLang).get.word
    bot = set.find(_.language == targetLang).get.word
  } yield (top,bot)

  val alphabet = Index(pairs.map(_._1).flatten.toSet ++ pairs.map(_._2).flatten.toSet + '\0')
  val ged = new GeneralEditDistance(3,alphabet, i => -3.0 - i, i => -3 + i, (_,_) => -1)

  // (ch1,ch2) => count
  def optimizeCounts(): DenseVector[Double] = {
    val params = Iterator.iterate(ged.initialParameters) { params =>
        val stats = for ((top, bot) <- pairs.par) yield ged.sufficientStatistics(params, top, bot)
        val reduced = stats.par.reduce(_ + _)
        ged.makeParameters(Map(targetLang -> reduced)).iterator.next._2
    }.drop (3).next
    val stats = for ((top, bot) <- pairs.par) yield ged.sufficientStatistics(params, top, bot)
    val decodedCounts = stats.reduce(_ + _).decode
    val allCounts = for( arr <- decodedCounts; matrix <- arr ) yield matrix
    allCounts.reduceLeft(_ + _) + 0.1
  }

  val counts = optimizeCounts()

  def backgroundProcess(time: Double, insertionRate: Double = 1E-4, substitutionRate: Double = 1E-4) = {
    val subRatio = {
      val base = log1p(-exp(-substitutionRate * time))
      -log1p(exp(-substitutionRate * time - base))
    }

    val insRatio = {
      val base = log1p(-exp(-insertionRate * time))
      -log1p(exp(-insertionRate * time - base))
    }

    new GeneralEditDistance(1,alphabet,_ => subRatio, _ => insRatio, (_,_) => 0.0)
  }

  val MAX_TIME = 1500

  case class MutationEvent(s: String,r: String, time: Double)
  val mult = new Multinomial(counts)

  val randString = for {
    len <- new Poisson(0.1)
  } yield Rand.choose(alphabet).sample(len).mkString

  val randMutation = for {
    s <- randString
    r <- randString
    base <- mult
    (parCh,childCh) = ged.pairEncoder.decodeChar(base)
    if(parCh != childCh)
    if s.length > 0 || parCh != '\0'
    t <- Rand.uniform
  } yield MutationEvent(if(parCh == '\0') s else s + parCh, if(childCh == '\0') r else r + childCh,t * MAX_TIME)

  def computeLL(events: ArrayBuffer[MutationEvent]):Double = {
    val sorted = events.sortBy(_.time)
    val ed = if(events.nonEmpty) backgroundProcess(MAX_TIME) else backgroundProcess(MAX_TIME)
    def applyEvent(w: String, event: MutationEvent) = w.replaceAll(event.s,event.r)

    def doPair(latin: Word, spanish: Word):Double = {
      val preSpanish = sorted.foldLeft(latin)(applyEvent)
      val ll = ed.distance(ed.initialParameters,preSpanish,spanish)
      ll
    }
    val lls = for( (top,bot) <- pairs.par) yield doPair(top,bot)
    lls.sum
  }

  def priorPoints(n: Int) = -lgamma(n+1)

  val events = new ArrayBuffer[MutationEvent]();
  var ll = computeLL(events) + priorPoints(1)
  println(ll)
  for(iter <- 0 until 10000) {
    if(iter % 100 == 0) println(iter + " " + events)
    // try to add a point
    val m = randMutation.get
    events += m
    println("?" + m)
    val new_ll = computeLL(events) + priorPoints(events.size)
    if(log(random) <= new_ll - ll) {
      ll = new_ll
      println("+" + m + " " + new_ll + " " + ll)
    } else {
      events.trimEnd(1)
      println("!"+m + " " + new_ll + " " + ll)
    }

    // try to remove a point
    if(events.nonEmpty) {
      val i = Rand.randInt(events.size).get
      val m = events.remove(i)
      val new_ll = computeLL(events) + priorPoints(events.size)
      if(log(random) <= new_ll - ll) {
        ll = new_ll
        println("-" + m + " " + new_ll)
      } else {
        events += m
        println("~"+m)
      }
    }
  }

  println(events.sortBy(_.time))

}