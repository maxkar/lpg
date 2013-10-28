package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Javascript file. */
final class JSFile private[model](
    extGlobals : Iterable[String],
    vars : Seq[String],
    funcs :Seq[(String, FunctionBody)],
    inits :Seq[Statement]) {

  /** Writes to a context. */
  private def writeTo(ctx : CompactContext) : Unit = {
    /* Write variables. */
    if (!vars.isEmpty) {
      ctx.write("var ")
      val viter = vars.iterator
      ctx.write(viter.next)
      while (viter.hasNext) {
        ctx.writeChar(',')
        ctx.write(viter.next)
      }
      ctx.writeChar(';')
    }
    funcs.foreach(x ⇒ writeFunc(ctx, x._1, x._2))

    inits.foreach(x ⇒  x.writeStatement(ctx))
  }


  /** Writes context of this file into a writer. */
  def writeToWriter(w : java.io.Writer) : Unit = {
    val ctx = CompactContext.forWriter(w,
      (extGlobals ++ vars ++ funcs.map(x ⇒  x._1)).toSet)
    writeTo(ctx)
  }


  /** Writes a function. */
  private def writeFunc(ctx : CompactContext, n : String, b : FunctionBody) : Unit = {
    ctx.write("function ")
    ctx.write(n)
    b.writeTo(ctx)
  }
}
