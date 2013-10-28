package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class AssignmentExpression(
      host : Expression, value : Expression, sign : String)
    extends Expression {

  private[model] val priority : Int = 17
  private[model] def canStartStatement() : Boolean = host.canStartStatement

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    host.writeExpression(ctx)
    ctx.write(sign)

    val bracket = value.priority > 17

    if (bracket)
      ctx.writeChar('(')
    value.writeExpression(ctx)
    if (bracket)
      ctx.writeChar(')')
  }

}
