package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** While statement. */
private[model] final class WhileStatement(cond : Expression, body : Seq[Statement])
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("while(")
    cond.writeExpression(ctx)
    ctx.writeChar(')')
    if (body.size != 1)
      ctx.writeChar('{')
    body.foreach(_.writeStatement(ctx))
    if (body.size != 1)
      ctx.writeChar('}')
  }
}
