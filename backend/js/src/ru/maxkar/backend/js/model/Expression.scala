package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Base trait for all expressions. */
trait Expression extends Statement {

  /** Expression priority. Items with a lower value binds earlier. */
  private[model] val priority : Int

  /** Checks if this expression is comma-safe. */
  private[model] val isCommaSafe : Boolean = true

  /** Checks if this expression is minus-safe. */
  private[model] val isMinusSafe : Boolean = true

  /** Checks whether this expression can start a statement. */
  private[model] val canStartStatement : Boolean = true

  /** Writes this element into the output in compact form.*/
  private[model] def writeExpression(ctx : CompactContext) : Unit

  /** Writes this expression after a minus sign.
   * Default implementation writes this expression as-is. */
  private[model] def writeExpressionAfterMinus(
      writer : CompactContext) : Unit = {
    if (!isMinusSafe)
      writer.write(' ')
    writeExpression(writer)
  }

  /** Writes an expression in comma-safe manner.
   * This method is called in context where comma should not occur in the
   * top level of the expression.
   */
  private[model] def writeExpressionCommaSafe(ctx : CompactContext) : Unit =
    ctx.bracketed(!isCommaSafe, '(', ')', writeExpression(ctx))

  /** Writes this expression as a member accessor.
   * This implementation uses a square brackets for the expression.
   */
  private[model] def writeAsMemberAccessor(ctx : CompactContext) : Unit = {
    ctx.write('[')
    writeExpression(ctx)
    ctx.write(']')
  }


  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.bracketed(!canStartStatement, '(', ')', writeExpression(ctx))
    ctx.write(';')
  }
}
