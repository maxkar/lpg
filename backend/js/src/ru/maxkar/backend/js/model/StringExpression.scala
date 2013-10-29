package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** String value expresion. */
private[model] final class StringExpression(
      value : String) extends Expression {

  val priority = 0
  override val canStartStatement = true

  private[model] def writeExpression(ctx : CompactContext) : Unit =
    ctx.write(Model.quoteString(value))

  override private[model] def writeAsMemberAccessor(ctx : CompactContext) : Unit = {
    if (!validSimpleId(ctx))
      super.writeAsMemberAccessor(ctx)
    else {
      ctx.write('.')
      ctx.write(value)
    }
  }

  /** Checks, if value is valid identifier. */
  private def validSimpleId(ctx : CompactContext) : Boolean = {
    if (value.isEmpty)
      return false
    if (Character.isDigit(value.charAt(0)))
      return false

    var ptr = 0
    while (ptr < value.length) {
      if (!ctx.isValidIdentifierChar(value.charAt(ptr)))
        return false
      ptr+=1
    }

    true
  }
}
