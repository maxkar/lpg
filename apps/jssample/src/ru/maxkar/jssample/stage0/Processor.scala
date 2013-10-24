package ru.maxkar.jssample.stage0

import ru.maxkar.hunk._
import ru.maxkar.hunk.Hunk._

import java.io._

import scala.collection.mutable.ArrayBuffer

import ru.maxkar.lispy.front.build._

import ru.maxkar.lispy.front._
import ru.maxkar.lispy.front.parser._
import ru.maxkar.lispy.front.stage0._

/** Processeor for the stage-0 items. */
final object Processor {

  /** Result type. */
  private type Res = SList[BaseItem]


  /** Inputs a file content. */
  private def getContent(input : File) : Hunk[Array[Char], Unit] = calc {
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
  private def parseContent(file : File)(content : Array[Char]) : Hunk[Res, Unit] = calc {
    val input = Input.fromCharArray(content)
    SParser.parseSFile(parseAttr)(input)
  }


  /** Processes one file. */
  private def processFile(input : File, path : Seq[String]) : Hunk[Res, Unit] = {
    val content = getContent(input)
    parseContent(input) _ <**> content
  }


  /** Processes a list of inputs into list of stage-0 items. */
  def process(inputs : Seq[String]) : Seq[Hunk[Res, Unit]] = {
    val res = new ArrayBuffer[Hunk[Res, Unit]]

    def acceptFile(f : File, path : Seq[String]) : Unit = {
      if (f.getName.endsWith(".lpg"))
        res += processFile(f, path)
    }

    inputs.foreach(i ⇒
        Files.scanFiltered(new File(i), acceptDirectory, acceptFile))

    res.toSeq
  }


  /** Directory acceptor. */
  private def acceptDirectory(f : File, path : Seq[String]) : Boolean =
    !f.getName.startsWith(".")
}
