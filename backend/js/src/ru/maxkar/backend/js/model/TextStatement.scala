package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Simple statement with a given text. */
private[model] final class TextStatement(text : String) extends Statement {
  private[model] def writeStatement(ctx : CompactContext) : Unit = {
    ctx.write(text)
    ctx.writeChar(';')
  }
}
