package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Instance creation expression. */
private[model] final class CallExpression(
    peer : NonprimitiveExpression, args : Seq[Expression])
      extends NonprimitiveExpression {

  private[model] val priority : Int = 0


  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    val useBracket = peer.priority > 2

    if (useBracket)
      ctx.writeChar('(')
    peer.writeExpression(ctx)
    if (useBracket)
      ctx.writeChar(')')

    ctx.writeChar('(')
    val itr = args.iterator
    if (itr.hasNext)
      itr.next.writeExpressionCommaSafe(ctx)
    while (itr.hasNext) {
      ctx.writeChar(',')
      itr.next.writeExpressionCommaSafe(ctx)
    }
    ctx.writeChar(')')
  }
}

