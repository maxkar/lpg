package ru.maxkar.scoping.simple

import scala.collection.mutable.ArrayBuffer

final class CollectingScopeBuilder[K, V <: AnyRef] private[simple]()
    extends ScopeBuilder[K, V] {

  /** Duplicate definitions. Key, first and next occurences. */
  private val dupes = new ArrayBuffer[(K, V, V)]


  private[simple] def onDupe(k : K, v1 : V, v2 : V) : Unit =
    dupes += ((k, v1, v2))


  /** Returns all found duplicates. */
  def duplicates() : Seq[(K, V, V)] = dupes
}
