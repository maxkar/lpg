package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class MemberAccessExpression(
      base : Expression, tail : Expression)
    extends NonprimitiveExpression with LeftValue {

  private[model] val priority : Int = 0

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    val needBrackets = base.priority > priority
    if (needBrackets)
      ctx.writeChar('(')
    base.writeExpression(ctx)
    if (needBrackets)
      ctx.writeChar('(')
    tail.writeAsMemberAccessor(ctx)
  }
}
