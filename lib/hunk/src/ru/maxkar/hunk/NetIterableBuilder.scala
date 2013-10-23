package ru.maxkar.hunk

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

/** Builder for the net iterables list. */
private[hunk] class NetIterableBuilder[T] {
  private[hunk] val items = new ArrayBuffer[Seq[T]]
  private val iset = new HashSet[NetIterable[T]]


  /** Populates staring with a given item. */
  private[hunk] def populate(item : NetIterable[T]) : Unit = {
    if (iset(item))
      return
    iset += item
    item.peers.foreach(populate)
    items += item.ownItems
  }
}
