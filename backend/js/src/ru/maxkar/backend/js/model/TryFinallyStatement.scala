package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext


/** Try/finally statement. */
private[model] final class TryFinallyStatement(
      body : Seq[Statement], fin : Seq[Statement])
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("try{")
    body.foreach(_.writeStatement(ctx))
    ctx.write("}finally{")
    fin.foreach(_.writeStatement(ctx))
    ctx.write("}")
  }
}
