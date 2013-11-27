package ru.maxkar.backend.js.out

/** Default generators. */
private[out] object CharGen {
  /** Type for the identifier generator. */
  private[out] type IdCharGen = () ⇒ Iterator[Char]


  /** First letter generator. */
  val firstLetter : IdCharGen = strGen(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")


  /** Milddle letters generator. */
  val middleLetter : IdCharGen = strGen(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")


  /** Generates a characters from the string. */
  private def strGen(str : String)() : Iterator[Char] =
    str.iterator
}
