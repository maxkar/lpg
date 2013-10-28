package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Object literal expression. */
private[model] final class ObjectExpression(items : Seq[(String, Expression)])
    extends Expression {

  private[model] val priority = 0
  private[model] def canStartStatement() : Boolean = false

  /** Writes one entry only. */
  private def writeEntry(ctx : CompactContext, entry : (String, Expression)) : Unit = {
    val (key, value) = entry
    ctx.write(
      if (keyIsGood(key))
        key
      else
        Model.quoteString(key))

    ctx.writeChar(':')
    value.writeExpressionCommaSafe(ctx)
  }


  /** Checks if key is good or not. */
  private def keyIsGood(key : String) : Boolean = {
    if (key.isEmpty)
      return false
    if (!Character.isJavaIdentifierStart(key.charAt(0)))
      return false
    var ptr = 1
    while (ptr < key.length) {
      if (!Character.isJavaIdentifierPart(key.charAt(ptr)))
        return false
    }

    true
  }


  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.writeChar('{')
    val itr = items.iterator

    if (itr.hasNext)
      writeEntry(ctx, itr.next)
    while (itr.hasNext) {
      ctx.writeChar(',')
      writeEntry(ctx, itr.next)
    }

    ctx.writeChar('}')
  }
}
