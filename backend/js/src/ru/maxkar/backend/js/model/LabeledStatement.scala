package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Statement with a label as an argument. */
private[model] final class LabeledStatement(
    text : String, lbl : AnyRef) extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write(text)
    ctx.writeChar(' ')
    ctx.write(ctx.resolveLabel(lbl))
    ctx.writeChar(';')
  }
}

