package ru.maxkar.jssample.platform

import ru.maxkar.scoping.simple._

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.jssample.out.Symbol
import ru.maxkar.jssample.ns._

/**
 * Platform specification.
 */
final class Platform(val items : Seq[(String, Symbol)]) {
  /** Platform scope. */
  val scope = {
    val sb = ScopeBuilder.collecting[String, Symbol]
    items.foreach(x ⇒  sb.offer(x._1, x._2))
    sb.scope
  }
}


/** Platform accessors. */
final object Platform {
  import java.io._
  import scala.collection.mutable.ArrayBuffer

  /** Creates a new platfrom from the given file. */
  def fromFile(f : File) : Platform = {
    val fis = new FileInputStream(f)
    val res = new ArrayBuffer[(String, Symbol)]
    var line = 1

    try {
      val br = new BufferedReader(
        new InputStreamReader(new BufferedInputStream(fis), "UTF-8"))

      var nline : String = br.readLine
      while (nline != null) {
        val rline = nline.trim
        if (!rline.isEmpty)
          res += ((rline, new Symbol(
            new ModuleHost(f, new TextPosition(line, 1)))))
        line += 1
        nline = br.readLine
      }

    } finally {
      fis.close
    }

    new Platform(res.toSeq)
  }
}
