package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Base trait for all expressions. */
trait Expression extends Statement {

  /** Expression priority. Items with a lower value binds earlier. */
  private[model] val priority : Int


  /** Writes this element into the output in compact form.*/
  private[model] def writeExpression(ctx : CompactContext) : Unit

  /** Writes this expression after a minus sign.
   * Default implementation writes this expression as-is. */
  private[model] def writeExpressionAfterMinus(
      writer : CompactContext) : Unit =
    writeExpression(writer)

  /** Writes an expression in comma-safe manner.
   * This method is called in context where comma should not occur in the
   * top level of the expression.
   * Default implementation just writes an expression as-is.
   */
  private[model] def writeExpressionCommaSafe(ctx : CompactContext) : Unit =
    writeExpression(ctx)

  /** Writes this expression as a member accessor.
   * This implementation uses a square brackets for the expression.
   */
  private[model] def writeAsMemberAccessor(ctx : CompactContext) : Unit = {
    ctx.writeChar('[')
    writeExpression(ctx)
    ctx.writeChar(']')
  }


  private[model] def writeStatement(ctx : CompactContext) : Unit = {
     writeExpression(ctx)
     ctx.writeChar(';')
  }
}
