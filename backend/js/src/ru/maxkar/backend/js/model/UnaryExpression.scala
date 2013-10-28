package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class UnaryExpression(
    base : Expression, code : String)
      extends Expression {

  private[model] val priority = 4
  private[model] def canStartStatement() : Boolean = true

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.write(code)

    val useBracket = base.priority > 4
    if (useBracket)
      ctx.writeChar('(')
    base.writeExpression(ctx)
    if (useBracket)
      ctx.writeChar(')')
  }
}
