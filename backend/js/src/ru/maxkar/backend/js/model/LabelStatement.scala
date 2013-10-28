package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Labeled statement .*/
private[model] final class LabelStatement(lbl : AnyRef, body : Statement)
    extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write(ctx.resolveLabel(lbl))
    ctx.writeChar(':')
    body.writeStatement(ctx)
  }
}
