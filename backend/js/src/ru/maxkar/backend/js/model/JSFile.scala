package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/** Javascript file. */
final class JSFile private[model](
    extGlobals : Iterable[String],
    vars : Seq[String],
    pvars : Seq[AnyRef],
    funcs :Seq[(String, FunctionBody)],
    pfuncs :Seq[(AnyRef, FunctionBody)],
    inits :Seq[Statement]) {

  /** Writes to a context. */
  private def writeTo(ctx : CompactContext) : Unit = {
    val sc = ctx.sub(pvars ++ pfuncs.map(_._1), Seq.empty)

    /* Write private variables. */
    if (!pvars.isEmpty) {
      sc.write("var ")
      sc.sepby[AnyRef](pvars, ',', sc.writeVariable)
      sc.write(';')
    }

    /* Write global variables. */
    if (!vars.isEmpty) {
      sc.write("var ")
      sc.sepby[String](vars, ',', sc.write)
      sc.write(';')
    }

    pfuncs.foreach(x ⇒ {
        sc.write("function ")
        sc.writeVariable(x._1)
        x._2.writeTo(sc)
      });

    funcs.foreach(x ⇒ writeFunc(sc, x._1, x._2))
    inits.foreach(x ⇒  x.writeStatement(sc))
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
