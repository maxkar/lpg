package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class CommaExpression(
      left : Expression, right : Expression)
    extends Expression {

  private[model] val priority : Int = 18
  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    left.writeExpression(ctx)
    ctx.writeChar(',')
    right.writeExpression(ctx)
  }

  override private[model] def writeExpressionCommaSafe(
      ctx : CompactContext) : Unit = {
    ctx.writeChar('(')
    writeExpression(ctx)
    ctx.writeChar(')')
  }
}
