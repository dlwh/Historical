package dlwh.baldur

import scalanlp.config.Configuration
import java.io.File
import collection.{Seq, IndexedSeq}
import scalanlp.util.Index
import math._
import scalanlp.stats.distributions.{Poisson, Rand}
import collection.mutable.ArrayBuffer
import scalala.library.Numerics._
import scalanlp.math.Semiring.LogSpace._
import scalanlp.newfst.{Automaton, EditDistance}

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

  val words: IndexedSeq[Seq[Cognate]] = dataset.cognates.map(_.filter(cog => myLangs(cog.language))).filter(_.nonEmpty).take(10)

  val pairs = for {
    set <- words
    top = set.find(_.language == originLang).get.word
    bot = set.find(_.language == targetLang).get.word
  } yield (top,bot)

  val alphabet = Index(pairs.map(_._1).flatten.toSet ++ pairs.map(_._2).flatten.toSet)

  def backgroundProcess(time: Double, insertionRate: Double = 1E-4, substitutionRate: Double = 1E-4) = {
    val subRatio = {
      val base = log1p(-exp(-substitutionRate * time))
      -log1p(exp(-substitutionRate * time - base))
    }

    val insRatio = {
      val base = log1p(-exp(-insertionRate * time))
      -log1p(exp(-insertionRate * time - base))
    }

    new EditDistance(subRatio,insRatio,alphabet.toSet)
  }

  val MAX_TIME = 1500

  case class MutationEvent(s: String,r: String, time: Double)

  val randString = for {
    len <- new Poisson(0.1)
  } yield Rand.choose(alphabet).sample(len+1).mkString

  val randMutation = for {
    s <- randString
    r <- randString
    t <- Rand.uniform
  } yield MutationEvent(s,r,t * MAX_TIME)

  def computeLL(events: ArrayBuffer[MutationEvent]):Double = {
    val sorted = events.sortBy(_.time)
    val ed = if(events.nonEmpty) backgroundProcess(MAX_TIME) else backgroundProcess(MAX_TIME)
    def applyEvent(w: String, event: MutationEvent) = w.replaceAll(event.s,event.r)

    def doPair(latin: Word, spanish: Word):Double = {
      val preSpanish = sorted.foldLeft(latin)(applyEvent)
      val ll = (Automaton.constant(preSpanish,0.0).asTransducer >> ed >> Automaton.constant(spanish,0.0).asTransducer).cost
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
      println("+" + m + " " + new_ll)
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