package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** For statement. */
private[model] final class ForStatement(
      cond : Expression, update : Expression, body : Seq[Statement])
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("for(;")
    update.writeExpression(ctx)
    ctx.writeChar(';')
    update.writeExpression(ctx)
    ctx.writeChar(')')
    if (body.size != 1)
      ctx.writeChar('{')
    body.foreach(_.writeStatement(ctx))
    if (body.size != 1)
    ctx.writeChar('}')
  }
}
