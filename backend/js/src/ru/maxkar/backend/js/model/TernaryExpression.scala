package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class TernaryExpression(
      cond : Expression, onTrue : Expression, onFalse : Expression)
    extends Expression {
  private[model] val priority : Int = 15

  private[model] def canStartStatement() : Boolean =
    cond.priority >= priority || cond.canStartStatement

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    val lbracket = cond.priority >= priority
    val rbracket = onFalse.priority > priority

    if (lbracket)
      ctx.writeChar('(')
    cond.writeExpression(ctx)
    if (lbracket)
      ctx.writeChar(')')

    ctx.writeChar('?')
    onTrue.writeExpression(ctx)
    ctx.writeChar(':')

    if (rbracket)
      ctx.writeChar('(')
    onFalse.writeExpression(ctx)
    if (rbracket)
      ctx.writeChar(')')
  }
}

