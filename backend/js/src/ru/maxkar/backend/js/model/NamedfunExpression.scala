package ru.maxkar.backend.js.model


import ru.maxkar.backend.js.out.CompactContext

/** Anonymous function expression. */
private[model] final class NamedfunExpression(selfid : AnyRef, body : FunctionBody)
    extends Expression {
  private[model] val priority = 0

  private[model] def writeExpression(baseCtx : CompactContext) : Unit = {
    val ctx = baseCtx.sub(Set(selfid), Set.empty)
    ctx.write("function ")
    ctx.write(ctx.resolveVariable(selfid))
    body.writeTo(ctx)
  }
}

