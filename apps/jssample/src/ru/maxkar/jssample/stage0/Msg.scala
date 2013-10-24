package ru.maxkar.jssample.stage0


import java.io._

/** Stage-0 messages. */
final object Msg {
  def printException(stream : PrintStream, exn : Failure) : Unit = {
    exn match {
      case ReadFailure(f, e) ⇒
        stream.println(f)
        stream.println("ERROR: Failed to read file : " + exn)
        e.printStackTrace(stream)
    }
  }
}
