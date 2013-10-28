package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] final class DoWhileStatement(
      items : Seq[Statement], cond : Expression)
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("do{")
    items.foreach(_.writeStatement(ctx))
    ctx.write("}while(")
    cond.writeExpression(ctx)
    ctx.write(");")
  }
}
