package ru.maxkar.lispy.parser

/**
 * Trait which allows to "look ahead" in input. Does not
 * allow to change an input position by itself.
 */
trait Lookahead {
  /** Checks, if an EOF is reached. */
  def atEof() : Boolean


  /** Peeks a next character.
   * @return negative value if stream is at EOF
   *   or character code otherwise
   */
  final def peek() : Int = peekAt(0)


  /** Peeks at a character in a given position.
   * @return negative value if that position is outside
   *   of the stream.
   */
  def peekAt(idx : Int) : Int


  /** Checks, if input at current position starts
   * with a given (verbatim) text.
   */
  def atText(text : String) : Boolean
}
