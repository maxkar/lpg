package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class ForInStatement(
      iterator : LeftValue, container : Expression,
      body : Seq[Statement])
    extends Statement {

  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("for (")
    iterator.writeExpression(ctx)
    ctx.write(" in ")
    container.writeExpression(ctx)
    ctx.writeChar(')')
    if (body.size != 1)
      ctx.writeChar('{')
    body.foreach(_.writeStatement(ctx))
    if (body.size != 1)
      ctx.writeChar('}')
  }
}
