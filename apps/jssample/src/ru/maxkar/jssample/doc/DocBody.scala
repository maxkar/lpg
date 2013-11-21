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
}
