package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Prefix operation. */
private[model] final class PostfixOp(peer : LeftValue, op : String)
    extends Expression {

  private[model] val priority = 2

  private[model] def writeExpression(ctx : CompactContext) : Unit = {
    peer.writeExpression(ctx)
    ctx.write(op)
  }
}

