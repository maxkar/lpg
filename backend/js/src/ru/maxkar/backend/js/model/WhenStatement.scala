package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** "When" statement (short if). */
private[model] final class WhenStatement(cond : Expression,
      body : Seq[Statement])
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("if(")
    cond.writeExpression(ctx)
    ctx.write("){")
    body.foreach(_.writeStatement(ctx))
    ctx.writeChar('}')
  }
}
