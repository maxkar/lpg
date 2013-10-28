package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Simple expression implementation. Just a text to be
 * written and no more. */
private[model] class TextExpression(text : String)
    extends Expression {

  private[model] val priority = 0

  private[model] def writeExpression(ctx : CompactContext) : Unit =
    ctx.write(text)
}
