package ru.maxkar.backend.js.out.syn.naming

/**
 * Item-caching id-generator. Caches already-generated identifiers.
 * Allows user to clone current state and generate "same" identifiers
 * on an "unrelated" branch.
 * @param provider base id provider.
 */
final class CachingIdGenerator private(
    provider : () ⇒ String,
    private var node : CacheNode) {

  /** Generates next id. */
  def next() : String = {
    val res = node.value
    if (node.next == null)
      node.next = new CacheNode(provider(), null)
    node = node.next
    res
  }

  /** Creates a "Separate" generator at a same generation position. */
  def slice() : CachingIdGenerator =
    new CachingIdGenerator(provider, node)
}

/** Companion object for the id generator. */
object CachingIdGenerator {
  def apply(provider : () ⇒ String) : CachingIdGenerator =
    new CachingIdGenerator(provider, new CacheNode(provider(), null))
}

/** Cache node. One node in a linked list. */
private final class CacheNode(val value : String, var next : CacheNode)
