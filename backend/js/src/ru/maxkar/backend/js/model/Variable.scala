package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

class Variable private[model](private[model] val ref : AnyRef)
    extends NonprimitiveExpression with LeftValue {

  private[model] val priority = 0
  private[model] def canStartStatement() : Boolean = true

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.write(ctx.resolveVariable(ref))
  }
}

