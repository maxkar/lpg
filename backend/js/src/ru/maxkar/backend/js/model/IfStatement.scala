package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** If statement implementation. */
private[model] final class IfStatement(cond : Expression,
      onTrue : Seq[Statement], onFalse : Seq[Statement])
    extends Statement {

  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("if(")
    cond.writeExpression(ctx)
    ctx.writeChar(')')
    if (onTrue.size != 1)
      ctx.writeChar('{')
    onTrue.foreach(_.writeStatement(ctx))
    if (onTrue.size != 1)
      ctx.writeChar('}')

    ctx.write("else")

    if (onFalse.size != 1)
      ctx.writeChar('{')
    else
      ctx.writeChar(' ')
    onFalse.foreach(_.writeStatement(ctx))
    if (onFalse.size != 1)
      ctx.writeChar('}')
  }
}
