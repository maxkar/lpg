package ru.maxkar.jssample.stage0

import ru.maxkar.jssample.msg.CollectorTrace

import ru.maxkar.jssample._

import ru.maxkar.hunk._
import ru.maxkar.hunk.Hunk._

import java.io._

import java.util.concurrent.Executor

import scala.collection.mutable.ArrayBuffer

/** Processeor for the stage-0 items. */
final class Processor(implicit executor : Executor) {


  /** Result type. */
  private type Res = (CollectorTrace, Option[Item])


  /** Processes one file. */
  private def processFile(input : File, path : Seq[String]) : Hunk[Res] = {
    val msg = new CollectorTrace(input)
    val chars = HostLoader.read(input, msg)

    exec {
      (msg, chars.flatMap(c ⇒
        HostLoader.parse(c, msg).map(sex ⇒
          Item(input, path, sex))))

    }
  }



  /** Processes a list of inputs into list of stage-0 items. */
  def process(inputs : Seq[String]) : Seq[Hunk[Res]] = {
    val res = new ArrayBuffer[Hunk[Res]]

    def acceptFile(f : File, path : Seq[String]) : Unit = {
      if (!f.getName.endsWith(".lpg"))
        return
      if (path.isEmpty)
        return
      val left = path.last
      val modname = left.substring(0, left.length - 4)
      val rpath = path.dropRight(1) :+ modname
      res += processFile(f, rpath)
    }

    inputs.foreach(i ⇒
        Files.scanFiltered(new File(i), acceptDirectory, acceptFile))

    res.toSeq
  }



  /** Directory acceptor. */
  private def acceptDirectory(f : File, path : Seq[String]) : Boolean =
    !f.getName.startsWith(".")
}
