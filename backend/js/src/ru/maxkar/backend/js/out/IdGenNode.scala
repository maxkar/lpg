package ru.maxkar.backend.js.out

/** Id generation node. */
private[out] final class IdGenNode(
    val name : String,
    generator : IdGenerator) {

  /** Cached next node. */
  private var cnext : IdGenNode = null

  /** Generates a next identifier node. */
  def next() : IdGenNode = {
    if (cnext == null)
      cnext = generator.genNext()
    cnext
  }
}

/** Id generator utils. */
private[out] final object IdGenNode {
  def default(blacklist : String ⇒  Boolean) : IdGenNode =
    IdGenerator.default(blacklist).genNext
}
