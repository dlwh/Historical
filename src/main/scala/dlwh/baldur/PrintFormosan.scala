package dlwh.baldur

import scalanlp.config.Configuration
import java.io.File
import java.util.Iterator

/**
 * 
 * @author dlwh

object PrintFormosan extends App {
  val configuration = Configuration.fromPropertiesFiles(args.map(new File(_)))

  val dataset = Dataset.fromConfiguration(configuration)

  val languages = Set("Paiwan","Rukai","SquliqAtay","CiuliAtaya")

  val leaves = dataset.tree.leaves.toSet


  val targets = for( group <- dataset.cognates) yield for(c <- group if leaves(c.language)) yield c
//  val targets = for( group <- dataset.cognates; c <- group) yield c
//  println(targets.groupBy(_.language).minBy(_._2.length))
//  targets.filter(c => leaves(c.language)).groupBy(_.language).mapValues(_.length).toIndexedSeq.sortBy((x: (String,Int)) => x._2) foreach println
  targets filter (_.nonEmpty) foreach println
  println(targets.flatten.groupBy(_.language).mapValues(_.size))


} */

