package ru.maxkar.backend.js.out.syn

/**
 * "Code fragment" typeclass for the compact writer.
 * @param F type of the fragment.
 */
trait CompactFragmenter[F] {
  /** Converts string to a fragment. */
  def fromString(value : String) : F

  /** Composes several fragments into one. */
  def compose(fragments : Seq[F]) : F
}
