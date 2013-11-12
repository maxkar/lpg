package ru.maxkar.jssample.msg

final class CollectorTrace (protected val host : java.io.File)
    extends HostTrace{

  /** Collected messages. */
  private var msgs = new scala.collection.mutable.ArrayBuffer[Message]


  protected def msg(message : Message) : Unit =
    msgs += message



  /** Returns all collected messages. */
  def messages() : Seq[Message] = msgs


  /** Returns only error messages. */
  def errors() : Seq[Message] = messages
}
