package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Prefix operation. */
private[model] final class PrefixDecrement(peer : LeftValue)
    extends Expression {

  private[model] val priority = 2

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.write("--")
    peer.writeExpression(ctx)
  }

  override private[model] def writeExpressionAfterMinus(
      writer : CompactContext) : Unit = {
    writer.writeChar(' ')
    writeExpression(writer)
  }
}

