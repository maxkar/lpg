package ru.maxkar.backend.js.model


import ru.maxkar.backend.js.out.CompactContext

private[model] final class UnaryMinusExpression(
    base : Expression)
      extends Expression {

  private[model] val priority = 4
  private[model] def canStartStatement() : Boolean = true

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.writeChar('-')

    val useBracket = base.priority > 4
    if (useBracket)
      ctx.writeChar('(')
    base.writeExpressionAfterMinus(ctx)
    if (useBracket)
      ctx.writeChar(')')
  }

  override private[model] def writeExpressionAfterMinus(
      writer : CompactContext) : Unit = {
    writer.writeChar(' ')
    writeExpression(writer)
  }
}
