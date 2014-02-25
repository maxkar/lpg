package ru.maxkar.backend.js.out.syn

/**
 * Representation of expression for the compact syntax.
 * @param F type of the output fragment.
 * @param writer expression writer.
 * @param priority expression priority.
 * @param commaSafe flag, indicating that this expression will not conflict
 *   with surrounding commas.
 * @param minusSafe flag, indicating that this expression will not conglict
 *   with a prefix (binary or unary) minus.
 * @param statementStart flag, indicating that this expression can start
 *   statement. For example, object literal cannot start an expression.
 */
private [syn] final case class CompactExpression[F](
  writer : F,
  priority : Int,
  commaSafe : Boolean,
  minusSafe : Boolean,
  statementStart : Boolean
)
