package ru.maxkar.scoping.simple


/** Scope builder with a callback handler. */
private[simple] final class CallbackScopeBuilder[K, V <: AnyRef](
      cb : (K, V, V) ⇒ Unit)
    extends ScopeBuilder[K, V] {

  private[simple] def onDupe(k : K, v1 : V, v2 : V) : Unit =
    cb(k, v1, v2)
}
