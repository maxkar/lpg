package ru.maxkar.backend.js.out.syn

/**
 * Statement definition for compact syntax.
 * @param F type of output fragment.
 */
private [syn] final case class CompactStatement[F] (
  writer : F
)