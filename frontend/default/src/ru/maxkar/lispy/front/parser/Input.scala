package ru.maxkar.lispy.front.parser

import ru.maxkar.lispy.front.Attribute
import ru.maxkar.lispy.front.Attributes

import scala.collection.mutable.ArrayBuffer

/**
 * Base trait for parser input stream. Input instances are mutable.
 * However, some parsers may use backtracking/checking,
 * so each input should be able to provide it's own copies.
 * These copies should be "unrelated". Reading from one copy
 * <b>must not</b> advance any other copies.
 * <p>Instances of the input are not safe to use from
 * different threads. Moreover, it is not safe to use
 * clones of one input in different threads.
 */
trait Input extends Lookahead {

  /**
   * Drops chars while a predicate is satisfied.
   * @param pred predicate, returns true when char should be dropped.
   * @return true iff at least one character was dropped.
   */
  def dropChars(pred : Char ⇒ Boolean) : Boolean


  /**
   * Drops a number of characters.
   * @param n number to drop.
   */
  def dropN(count : Int) : Unit


  /** Drops all whitespace characters. */
  final def dropWhites() : Unit =
    dropChars(Character.isWhitespace)



  /** Inputs chars while predicate is true.  */
  def charsWhile(pred : Char ⇒  Boolean) : String


  /** Attributes for element opening. */
  def openingAttributes() : Attributes


  /** Closing/ending attributes. Notes the same position
   * as openingAttributes but with other attributes' values
   */
  def closingAttributes() : Attributes
}



/** Input handlers, factories, etc... */
object Input {


  /** Starting offset in characters. */
  val startCharOffset : Attribute[Int] =
    new Attribute[Int]("Starting char offset")


  /** Starting offset in characters. */
  val endCharOffset : Attribute[Int] =
    new Attribute[Int]("Ending char offset")


  /** Source file. */
  val sourceFile : Attribute[java.io.File] =
    new Attribute[java.io.File]("Source file")


  /** Layouts a character array. */
  def layoutOfArray(items : Array[Char]) : TextLayout = {
    val buffer = new ArrayBuffer[Int]

    var ptr = 0
    while (ptr < items.length) {
      items(ptr) match {
        case '\n' ⇒ buffer += ptr
        case '\r' ⇒
          if (ptr + 1 == items.length || items(ptr + 1) != '\n')
            buffer += ptr
        case _ ⇒  ()
      }

      ptr += 1
    }

    new TextLayout(buffer.toArray)
  }


  /** Creates a new input from a char array.
   * Returned input provides only CharOffset as a location.
   * @param stream initial input stream.
   * @return stream which uses an array as the input base.
   */
  def fromCharArray(stream : Array[Char]) : Input =
    new CharArrayInput(stream, 0)
}
