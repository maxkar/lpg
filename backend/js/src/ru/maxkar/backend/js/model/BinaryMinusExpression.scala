package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class BinaryMinusExpression(
      left : Expression, right : Expression)
    extends Expression {

  private[model] val priority : Int = 6
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

    ctx.writeChar('-')

    if (rbracket)
      ctx.writeChar('(')
    right.writeExpressionAfterMinus(ctx)
    if (rbracket)
      ctx.writeChar(')')
  }

}

