package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.syn.CompactFragmenter
import java.io.Writer

/** Writeable fragment implementation. */
object WriteClass extends CompactFragmenter[Writer ⇒ Unit] {
  override def fromString(value : String) : Writer ⇒ Unit =
    w ⇒ w.write(value)

  override def compose(items : Seq[Writer ⇒ Unit]) : Writer ⇒ Unit =
    w ⇒ items.foreach(_(w))
}
