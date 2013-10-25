package ru.maxkar.jssample.stage1

import java.io.PrintStream
import java.io.File

/** Stage1 messaging subsystem. */
final object Msg {
  import ru.maxkar.jssample.MessageFormat._


  /** Prints fatal information from anamnesis. */
  def printErrors(stream : PrintStream, id : File,
      anamnesis: Anamnesis1) : Unit = {
    anamnesis.duplicateDeclarations.foreach(x ⇒
      stream.println(err(id, x.next,
        "Duplicate declaration of " + x.name + ", first occurence at " +
        formatLocation(x.first))))
  }
}
