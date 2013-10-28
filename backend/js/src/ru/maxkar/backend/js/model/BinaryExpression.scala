package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class BinaryExpression(
      left : Expression, right : Expression, sign : String,
      private[model] val priority : Int)
    extends Expression {

  private[model] def canStartStatement() : Boolean =
    left.priority > priority || left.canStartStatement

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    val lbracket = left.priority > priority
    val rbracket = right.priority >= priority

    if (lbracket)
      ctx.writeChar('(')
    left.writeExpression(ctx)
    if (lbracket)
      ctx.writeChar(')')

    ctx.write(sign)

    if (rbracket)
      ctx.writeChar('(')
    right.writeExpression(ctx)
    if (rbracket)
      ctx.writeChar(')')
  }

}
