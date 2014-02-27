package ru.maxkar.backend.js.out.syn.naming

/**
 * Name generator which keeps respect to some "forbidden" names.
 * @param base base identifier generator.
 * @param blacklist blacklisted names (such as keywords,
 *  platform or exported names).
 */
final class TamedNameGenerator(
    base : WildNameGenerator,
    blacklist : Set[String]) {

  /** Generates next identifier. */
  def next() : String = {
    while (true) {
      val res = base.next
      if (!isBad(res))
        return res
    }
    ""
  }

  /** Checks if this identifier is acceptable or not. */
  private def isBad(id : String) : Boolean =
    blacklist(id)
}
