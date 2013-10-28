package ru.maxkar.backend.js.out

import CharGen._

/** Identifier generator. */
private[out] final class IdGenerator(
    first : IdCharGen, middle : IdCharGen,
    blacklist: String ⇒ Boolean) {

  /** Current generators. */
  var cgens = new Array[Iterator[Char]](0)
  /** Generated content. */
  var content = new Array[Char](0)


  /** Generates a next non-blacklisted identifier node. */
  def genNext() : IdGenNode = {
    var res = genInternal
    while (blacklist(res.name))
      res = genInternal
    res
  }


  /** Generates an internal identifier node.
   * May return blacklisted items. */
  private def genInternal() : IdGenNode = {
    val g1 = genCurSize
    if (g1 != null)
      return g1
    grow

    new IdGenNode(new String(content), this)
  }


  /** Reallocates for a new generation. */
  private def grow() : Unit = {
    val nlen = cgens.length + 1
    cgens = new Array[Iterator[Char]](nlen)
    content = new Array[Char](nlen)

    cgens(0) = first()
    content(0) = cgens(0).next

    var ptr = 1
    while (ptr < nlen) {
      cgens(ptr) = middle()
      content(ptr) = cgens(ptr).next
      ptr += 1
    }
  }


  /** Generates at the same length.
   * Returns null if no more id's can be
   * generated with a current length. */
  private def genCurSize() : IdGenNode = {

    var cptr = cgens.length - 1
    while (cptr >= 0) {
      val ccgen = cgens(cptr)
      if (ccgen.hasNext) {
        content(cptr) = ccgen.next
        return new IdGenNode(new String(content), this)
      }

      /* Reset generator. */
      val regen =
        if (cptr == 0)
          first
        else
          middle
      cgens(cptr) = regen()
      content(cptr) = cgens(cptr).next

      cptr -= 1
    }

    null
  }
}


/** Identifier generator accessor. */
private[out] final object IdGenerator {
  /** Creates a default id generator with a given blacklist. */
  def default(blacklist : String ⇒ Boolean) : IdGenerator =
    new IdGenerator(CharGen.firstLetter, CharGen.middleLetter, blacklist)
}
