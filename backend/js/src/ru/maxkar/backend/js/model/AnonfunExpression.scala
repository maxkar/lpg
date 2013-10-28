package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Anonymous function expression. */
private[model] final class AnonfunExpression(body : FunctionBody)
    extends Expression {
  private[model] val priority = 0
  private[model] def canStartStatement() : Boolean = false

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.write("function")
    body.writeTo(ctx)
  }
}
