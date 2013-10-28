package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Prefix operation. */
private[model] final class PrefixOp(peer : LeftValue, op : String)
    extends Expression {

  private[model] val priority = 2

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    ctx.write(op)
    peer.writeExpression(ctx)
  }
}
