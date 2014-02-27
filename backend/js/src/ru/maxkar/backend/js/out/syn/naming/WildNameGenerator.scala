package ru.maxkar.backend.js.out.syn.naming

import scala.collection.mutable.ArrayBuffer

/**
 * Untamed names generator. Generates names according to
 * specific "charsets" and does not check name validity.
 * @param first possible values for a first char.
 * @param other possible values for other characters.
 */
final class WildNameGenerator(
    first : Iterable[Char],
    other : Iterable[Char]) {

  /** Current content (id chars). */
  private var content = new StringBuilder

  /** Iterators used for value generaton. */
  private var generators = new ArrayBuffer[Iterator[Char]]

  /** Generates a next "possible identifier". */
  def next() : String = {
    if (!rollInplace())
      grow()
    content.toString
  }

  /** Attempts to "roll" identifier inplace. If last
   * ID is reached, returns false and resets all existing
   * iterators to it's initial state.
   * @returns true iff new value was generated inplace.
   */
  private def rollInplace() : Boolean = {
    var ptr = generators.length - 1
    while (ptr >= 0) {
      if (rollPosition(ptr))
        return true
    }

    return false
  }

  /**
   * Rolls a one position only.
   * @return true iff position was rolled without an otherflow.
   */
  private def rollPosition(pos : Int) : Boolean = {
    val oldIter = generators(pos)
    if (oldIter.hasNext) {
      content.setCharAt(pos, oldIter.next)
      return true
    }

    val newIter = seqFor(pos)
    generators(pos) = newIter
    content.setCharAt(pos, newIter.next)
    false
  }

  /** Grows ID size. */
  private def grow() : Unit = {
    val iter = seqFor(generators.length)
    content.append(iter.next)
    generators += iter
  }

  /** Creates an iterator for the given position. */
  private def seqFor(position : Int) : Iterator[Char] =
    if (position == 0)
      first.iterator
    else
      other.iterator
}
