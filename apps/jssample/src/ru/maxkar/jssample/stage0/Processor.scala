package ru.maxkar.jssample.stage0

import ru.maxkar.jssample._

import ru.maxkar.hunk._
import ru.maxkar.hunk.Hunk._

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._

import java.io._

import java.util.concurrent.Executor

import scala.collection.mutable.ArrayBuffer

/** Processeor for the stage-0 items. */
final class Processor(implicit executor : Executor) {


  /** Result type. */
  private type Res = Item



  /** Inputs a file content. */
  private def getContent(input : File) : Hunk[Array[Char], Trace] = calc {
    try {
      Files.fileAsCharsEnc(input, "UTF-8")
    } catch {
      case e : IOException ⇒
        throw new ReadFailure(input, e)
    }
  }



  /** Parses attributes. */
  private def parseAttr(x : Input) : Attributes = {
    Attributes.empty
  }



  /** Parses a file content. */
  private def parseContent(file : File)(content : Array[Char]) : Hunk[SList[BaseItem], Trace] = exec {
    val input = Input.fromCharArray(content)
    try {
      SParser.parseSFile(parseAttr)(input)
    } catch {
      case e : SFormatException ⇒
        throw new SFormatFailure(file, e)
    }
  }



  /** Processes one file. */
  private def processFile(input : File, path : Seq[String]) : Hunk[Res, Trace] = {
    val content = getContent(input)
    val body = parseContent(input) _ <**> content
    Item.curried(input)(path) <*> body
  }



  /** Processes a list of inputs into list of stage-0 items. */
  def process(inputs : Seq[String]) : Seq[Hunk[Res, Trace]] = {
    val res = new ArrayBuffer[Hunk[Res, Trace]]

    def acceptFile(f : File, path : Seq[String]) : Unit = {
      if (path.isEmpty)
        return
      val left = path.last
      val modname = left.substring(0, left.length - 4)
      val rpath = path.dropRight(1) :+ modname
      if (f.getName.endsWith(".lpg"))
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
