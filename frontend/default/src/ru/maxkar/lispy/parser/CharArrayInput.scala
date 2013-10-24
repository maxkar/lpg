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
      stream : Array[Char], layout : TextLayout, var offset : Int)
    extends Input {


  override def atEof() : Boolean =
    offset == stream.length


  override def dropChars(pred : Char ⇒ Boolean) : Boolean = {
    val start = offset
    while (!atEof) {
      if (pred(stream(offset)))
        offset += 1
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
    if (count <= 0)
      return
    val dc = Math.min(count, stream.length - offset)
    offset += dc
  }


  override def charsWhile(pred : Char ⇒ Boolean) : String = {
    val start = offset
    while (!atEof && pred(stream(offset)))
      offset += 1
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
    singleton(Input.textPosition, layout(offset))
}
