package ru.maxkar.lispy.parser

/** Text layout information.
 * Holds mapping from character offsets to it's lines.
 */
final class TextLayout private[parser](linefeeds : Array[Int]) {

  /** Retreives a text location for the given character
   * offset.
   */
  def locationOf(offset : Int) : TextPosition = {
    val idx = java.util.Arrays.binarySearch(linefeeds, offset)
    if (idx == 0)
      return TextPosition(1, offset + 1)
    if (idx > 0)
      return TextPosition(idx + 1, offset - linefeeds(idx - 1))
    val ip = -(idx + 1)
    if (ip == 0)
      TextPosition(1, offset + 1)
    else
      TextPosition(ip + 1, offset - linefeeds(ip - 1))
  }


  /** Simple shortcut to location method. */
  def apply(offset : Int) : TextPosition = locationOf(offset)
}
