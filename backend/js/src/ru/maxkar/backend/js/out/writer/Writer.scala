package ru.maxkar.backend.js.out.writer

/**
 * Trait for writer type class.
 * @param T type of the "writer" object.
 */
trait Writer[T] {
  /** Creates a new writable from the string. */
  def token(x : String) : T

  /** Composes a sequence from from items. */
  def seq(items : T*) : T
}
