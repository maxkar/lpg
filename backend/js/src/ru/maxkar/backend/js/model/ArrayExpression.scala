package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Array expression literal. */
private[model] final class ArrayExpression(items : Seq[Expression])
    extends Expression {

  private[model] val priority = 0
  private[model] def canStartStatement() : Boolean = true

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.writeChar('[')

    val itr = items.iterator
    if (itr.hasNext)
      itr.next.writeExpressionCommaSafe(ctx)
    while (itr.hasNext) {
      ctx.writeChar(',')
      itr.next.writeExpressionCommaSafe(ctx)
    }

    ctx.writeChar(']')
  }
}
