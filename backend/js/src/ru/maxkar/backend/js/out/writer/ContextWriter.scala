package ru.maxkar.backend.js.out.writer

/**
 * Writer to a given context.
 * @param C type of the context.
 * @param printer item printer for the given context.
 */
final class ContextWriter[C](printer : (String, C) ⇒ Unit)
    extends Writer[C ⇒  Unit] {

  override def token(tok : String) : C ⇒  Unit = printer(tok, _)
  override def seq(items : (C ⇒  Unit)*) : C ⇒  Unit =
    c ⇒  items.foreach(_(c))
}
