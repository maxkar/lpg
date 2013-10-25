package ru.maxkar.jssample.stage2

import ru.maxkar.jssample.{stage1 ⇒ S1}
import ru.maxkar.jssample.ns._
import ru.maxkar.scoping._

import scala.collection.JavaConversions._

import java.io._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/** Stage-3 processor. */
final class Processor {
  /** Processes an entry. */
  def process(entries : Seq[S1.Result]) : Result = {
    val res = new HashMap[Seq[String], Namespace[Reference]]
    val defs = new HashMap[Seq[String], File]
    val warns = new ArrayBuffer[DuplicateModuleDefinition]

    entries.foreach(x ⇒ {
      val host = ModuleScope(x.module)
      val rmap = x.decls.toSeq.map(
          e ⇒ (e, RefNSMember(e, host))).toMap
      val ns = Namespace.fromMap[Reference](rmap)
      defs.put(x.module, x.source) match {
        case None ⇒ res.put(x.module, ns)
        case Some(dup) ⇒
          defs.put(x.module, dup)
          warns += DuplicateModuleDefinition(x.module, dup, x.source)
      }
    })

    new Result(res.toMap, new Anamnesis2(warns))
  }
}
