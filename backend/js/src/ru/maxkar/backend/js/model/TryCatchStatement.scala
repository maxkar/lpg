package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Try/catch statement. */
private[model] final class TryCatchStatement(
      body : Seq[Statement],
      exnId : AnyRef,
      exnHandler : Seq[Statement])
    extends Statement {

  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write("try{")
    body.foreach(_.writeStatement(ctx))
    ctx.write("}catch(")

    val ss = ctx.sub(Set(exnId), Seq.empty)
    ss.write(ss.resolveVariable(exnId))
    ss.write("){")
    exnHandler.foreach(_.writeStatement(ss))
    ss.write("}")
  }
}
