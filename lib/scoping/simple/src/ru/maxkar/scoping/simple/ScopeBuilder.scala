package ru.maxkar.scoping.simple

import scala.collection.mutable.ArrayBuffer

/** Builder for the simple scope. Allows to build new scope in
 * "mutable" way by offering items into it.
 * <p> Instances of this class are not thread-safe.
 * @param K type of the scope key.
 * @param V type of the scope value.
 */
abstract class ScopeBuilder[K, V <: AnyRef] private[simple]()  {
  /** Local (offered) definitions. */
  private val defs = new java.util.HashMap[K, V]


  /** Scope implementation. */
  private val scopeImpl = new Scope[K, V] {
    def lookup(key : K) : Set[V] = {
      val guess = defs.get(key)
      if (guess == null)
        Set.empty
      else
        Set(guess)
    }
  }


  /** Offers new item into this scope. If there is no item
   * for the key yet, adds item into a scope. If there is an item
   * for the key, records new object as a dupe if it differs from an
   * initial object.
   */
  def offer(key : K, item : V) : Unit = {
    val prev = defs.put(key, item)
    if (prev == null || prev == item)
      return
    defs.put(key, prev)
    onDupe(key, prev, item)
  }

  /** Handles a duplicate definition. */
  private[simple] def onDupe(key : K, prev : V, next : V) : Unit


  /** Returns scope bound to this bulider. Adding new items
   * to this builder will effectively add new items into
   * the returned scope.
   */
  def scope() : Scope[K, V] = scopeImpl


  /** All registered entries. */
  def entries() : java.util.Map[K, V] =
    java.util.Collections.unmodifiableMap(defs)
}


/** Scope builder factoriy. */
final object ScopeBuilder {
  /** Creates a new scope builder which invokes callback on duplicate entries. */
  def withCallback[K, V <: AnyRef](
      cb : (K, V, V) ⇒ Unit) : ScopeBuilder[K, V] =
    new CallbackScopeBuilder(cb)


  /** Creates a new scope builder which collects all errors into a list. */
  def collecting[K, V <: AnyRef]() : CollectingScopeBuilder[K, V] =
    new CollectingScopeBuilder()
}
