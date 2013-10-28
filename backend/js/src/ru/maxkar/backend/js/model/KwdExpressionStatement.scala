package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext
/** Keyword+expressions statement. */
private[model] final class KwdExpressionStatement(
      kwd :String, expr : Expression)
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write(kwd)
    ctx.writeChar(' ')
    expr.writeExpression(ctx)
    ctx.writeChar(';')
  }
}
