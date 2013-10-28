package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

private[model] class Global(text : String)
    extends NonprimitiveExpression with LeftValue {

  private[model] val priority = 0

  private[model] def canStartStatement() : Boolean = true

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.ensureGlobal(text)
    ctx.write(text)
  }
}
