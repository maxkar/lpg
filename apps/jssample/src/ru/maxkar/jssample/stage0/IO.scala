package ru.maxkar.jssample.stage0

import java.io._


/** Input/output utilitiles. */
object IO {
  /** Copies characters using provides buffer.
   * @param buf temporary reading buffer
   * @param from source reader.
   * @param to target reader.
   */
  def copyCharsWithBuffer(
        buf : Array[Char],
        from : Reader, to :Writer) : Unit = {
    while (true) {
      val readed = from.read(buf)
      if (readed < 0)
        return
      to.write(buf, 0, readed)
    }
  }


  /** Copies all data from one stream to another. */
  def copyChars(from : Reader, to : Writer) : Unit =
    copyCharsWithBuffer(new Array[Char](4096 * 4), from, to)
}
