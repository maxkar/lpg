package ru.maxkar.jssample


import java.io._

/** Utilities to use with files during a
 * project build. This object provides only a
 * general-purpose utilities like scanning
 * directories and reading found files.
 */
object Files {
  /** Scans "accepted" directories and invokes callback
   * on all found files. Each callback receives a file among
   * it's local path (sequence of simple names starting from the
   * scan start point.
   * @param start scan starting point.
   * @param filter directory filter.
   * @param acceptor file acceptor.
   */
  def scanFiltered(
        start : File,
        filter : (File, Seq[String]) ⇒ Boolean,
        acceptor : (File, Seq[String]) ⇒  Unit) : Unit = {
    scanImpl(start, Seq.empty, filter, acceptor)
  }


  /** Scans all files in the subtree and invokes a callback. */
  def scanAll(start : File, acceptor : (File, Seq[String]) ⇒ Unit) : Unit =
    scanFiltered(start, acceptAll, acceptor)


  /** Reads file as a string. */
  def fileAsStringEnc(file : File, encoding : String) : String = {
    val fis = new FileInputStream(file)
    try {
      val fcs = new InputStreamReader(fis, encoding)
      try {
        val wrt = new StringWriter
        try {
          IO.copyChars(fcs, wrt)
        } finally {
          wrt.close
        }
        wrt.toString
      } finally {
        fcs.close
      }
    } finally {
      fis.close
    }
  }


  /** Reads file as a char array. */
  def fileAsCharsEnc(file : File, encoding : String) : Array[Char] = {
    val fis = new FileInputStream(file)
    try {
      val fcs = new InputStreamReader(fis, encoding)
      try {
        val wrt = new CharArrayWriter
        try {
          IO.copyChars(fcs, wrt)
        } finally {
          wrt.close
        }
        wrt.toCharArray
      } finally {
        fcs.close
      }
    } finally {
      fis.close
    }
  }


  /** Scan implementation. */
  private def scanImpl(
        start : File, path : Seq[String],
        filter : (File, Seq[String]) ⇒  Boolean,
        acceptor : (File, Seq[String]) ⇒  Unit) : Unit = {

    val subparts = start.listFiles
    if (subparts == null)
      acceptor(start, path)
    else if (filter(start, path))
      for (sf ← subparts)
        scanImpl(sf, path :+ sf.getName, filter, acceptor)
  }


  /** Accepts all. */
  private def acceptAll(file : File, path : Seq[String]) : Boolean = true
}
