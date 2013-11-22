package ru.maxkar.jssample.doc

import java.io.Writer

/** Documentation body. May contain formating data, etc...  */
abstract class DocBody private[doc]() {
  /** Outputs document into the stream. */
  private[doc] def writeTo(w : Writer) : Unit
}




/** Document body accessor, factories, combinators, etc... */
object DocBody {
  import scala.language.implicitConversions
  import com.coverity.security.Escape._


  /** Writes a document into a writer. */
  def write(doc : DocBody, w : Writer) : Unit =
    doc.writeTo(w)


  implicit def text(txt : String) : DocBody =
    new DocBody {
      def writeTo(w : Writer) : Unit =
        w.write(html(txt))
    }


  implicit def seq(items : Seq[DocBody]) : DocBody =
    new DocBody {
      def writeTo(w : Writer) : Unit =
        items.foreach(_.writeTo(w))
    }


  private def tagged(tag : String, item : DocBody) : DocBody =
    new DocBody {
      def writeTo(w : Writer) : Unit = {
        w.write("<")
        w.write(tag)
        w.write(">")
        item.writeTo(w)
        w.write("</")
        w.write(tag)
        w.write(">")
      }
    }


  def bold(item : DocBody) : DocBody = tagged("b", item)
  def italic(item : DocBody) : DocBody = tagged("i", item)
  def code(text : String) : DocBody =
    if (text.contains('\r') || text.contains('\n'))
      tagged("pre", tagged("code", text))
    else
      tagged("code", text)

  def p(item : DocBody) : DocBody = tagged("p", item)
  def br() : DocBody =
    new DocBody {
      def writeTo(w : Writer) : Unit =
        w.write("<br>")
    }

  def ol(items : Seq[DocBody]) : DocBody =
    tagged("ol", items.map(tagged("li", _)))
  def ul(items : Seq[DocBody]) : DocBody =
    tagged("ul", items.map(tagged("li", _)))
}
