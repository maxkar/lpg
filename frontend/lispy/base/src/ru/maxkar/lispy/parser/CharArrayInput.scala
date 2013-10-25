package ru.maxkar.lispy.parser

import ru.maxkar.lispy.Attribute
import ru.maxkar.lispy.Attributes
import ru.maxkar.lispy.Attributes._

/**
 * Character array input.
 * Source location is represented only as a
 * character offser attribute.
 */
private[parser] class CharArrayInput(
      stream : Array[Char], var offset : Int)
    extends Input {

  /** Current line. */
  private var line = 1
  /** Current column. */
  private var col = 1


  override def atEof() : Boolean =
    offset == stream.length


  override def dropChars(pred : Char ⇒ Boolean) : Boolean = {
    val start = offset
    while (!atEof) {
      if (pred(stream(offset)))
        skip1
      else
        return start < offset
    }
    start < offset
  }


  override def peekAt(idx : Int) : Int = {
    val tgt = offset + idx
    if (tgt >= stream.length)
      -1
    else
      stream(tgt)
  }


  override def dropN(count : Int) : Unit = {
    val limit = stream.length
    var c = count
    while (c > 0 && offset < limit) {
      skip1
      c -= 1
    }
  }


  override def charsWhile(pred : Char ⇒ Boolean) : String = {
    val start = offset
    while (!atEof && pred(stream(offset)))
      skip1
    new String(stream, start, offset - start)
  }


  override def atText(head : String) : Boolean = {
    if (head.length > stream.length - offset)
      return false

    var p1 = 0
    var p2 = offset

    while (p1 < head.length) {
      if (head.charAt(p1) != stream(p2))
        return false
      p1 += 1
      p2 += 1
    }

    true
  }


  override def location() : Attributes =
    singleton(Input.textPosition, new TextPosition(line, col))


  /** Skips one item. */
  private def skip1()  : Unit = {
    val poffset = offset
    offset += 1
    stream(poffset) match {
      case '\n' ⇒
        line += 1
        col = 1
      case '\r' ⇒
        if (offset < stream.length && stream(offset) == '\n')
          offset += 1
        line += 1
        col = 1
      case _ ⇒
        col += 1
    }
  }
}
