package ru.maxkar.jssample

import java.util.concurrent._

import ru.maxkar.lispy.Attribute

import ru.maxkar.jssample.msg._

import ru.maxkar.hunk.Hunk._
import ru.maxkar.hunk.Hunk

import ru.maxkar.scoping.simple._
import ru.maxkar.backend.js.model._

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

import java.io._


/** Application runner class. */
final object Runner {

  def main(args : Array[String]) : Unit = {
    if (args.length < 2)
      printUsageAndExit();

    implicit val executor =
      Executors.newFixedThreadPool(
        Runtime.getRuntime().availableProcessors)

    val boot = (new stage0.Processor).process(args.tail)

    val s0succs = mowait(boot)

    val s1runs = s0succs.map(x ⇒  exec {
        (x._1, out.Premodule.precompile(x._1, x._2.source, x._2.body))})

    val s1succs = mwait(s1runs)

    val globScope = createGlobScope(s1succs.map(_._2))

    val s2runs = s1succs.map(x ⇒  exec {
        (x._1, x._2.compile(globScope, x._1))
      })

    val s2succs = mwait(s2runs)

    val resf = collectjs(s2succs.map(_._2))

    try {
      val f = new BufferedWriter(new FileWriter(args(0)))
      try {
        resf.writeToWriter(f)
      } finally {
        f.close
      }
    } catch {
      case e : Throwable ⇒
        System.err.println("Fatal writing error: " + e)
        e.printStackTrace(System.err)
    }

    executor.shutdownNow
  }


  /**
   * Awaits items. If there is any error, then prints that error
   * and exit. Otherwise returns list of results.
   */
  private def ewait[T](items : Seq[Hunk[T]]) : Seq[T] = {
    val (fails, succs) = awaitSplit(items)

    fails.foreach(printFail)
    if (!fails.isEmpty)
      System.exit(2)

    succs
  }


  /** Awaits items with a possible failure. Unwraps options.
   * Stops when there is any error message or */
  private def mowait[T](items : Seq[Hunk[(CollectorTrace, Option[T])]]) : Seq[(CollectorTrace, T)] = {

    val snds = ewait(items)
    val errs = new ArrayBuffer[Message]
    val res = new ArrayBuffer[(CollectorTrace, T)]

    for (i ← snds) {
      val ierrs = i._1.errors
      if (!ierrs.isEmpty)
        errs ++= ierrs
      else
        i._2 match {
          case Some(x) ⇒ res += ((i._1, x))
          case None ⇒
            System.err.println("FATAL: Internal error: No result and no trace!")
            System.exit(312)
        }
    }

    errs.foreach(Message.printDefault(System.err, _))
    if (!errs.isEmpty)
      System.exit(2)

    res
  }


  /** Awaits items with a guaranteed result.
   * Stops when there is any error message or */
  private def mwait[T](items : Seq[Hunk[(CollectorTrace, T)]]) : Seq[(CollectorTrace, T)] = {

    val snds = ewait(items)
    val errs = snds.flatMap(x ⇒ x._1.errors)
    errs.foreach(Message.printDefault(System.err, _))
    if (!errs.isEmpty)
      System.exit(2)

    snds
  }


  /** Collects a js object. */
  private def collectjs(
        items : Seq[(Set[String], Seq[(String, FunctionBody)], Seq[Statement])])
      : JSFile = {

    var a = Set.empty[String]
    var b = Seq.empty[(String, FunctionBody)]
    var c = Seq.empty[Statement]

    items.foreach(x ⇒  {
        a ++= x._1
        b ++= x._2
        c ++= x._3
      })

    Model.file(Seq.empty, a.toSeq, Seq.empty, b, Seq.empty, c)
  }


  /** Creates a global scope. */
  private def createGlobScope(scopes : Seq[out.Premodule]) : Scope[String, out.Symbol] = {
    var gsb = ScopeBuilder.collecting[String, out.Symbol]

    for (s ← scopes)
      for (e ← s.globals)
        gsb.offer(e._1, e._2)

    val errs = gsb.duplicates

    for ((n, i1, i2) ← errs)
      System.err.println(MessageFormat.err(i2.declaration.file,
        i2.declaration.offset, "Duplicate definition of global " + n +
        ", previous declaration at\n  " +
        i1.declaration.file + ":" +
        MessageFormat.formatLocation(i1.declaration.offset)))

    if (!errs.isEmpty)
      System.exit(3)

    gsb.scope
  }


  /** Prints fails and exits. Exists only when at least on
   * failure exists.
   */
  private def printFailsAndExit(x : Seq[Throwable]) : Unit = {
    if (x.isEmpty)
      return
    x.foreach(printFail)
    System.exit(2)
  }


  /** Prints one exception. */
  private def printFail(f : Throwable) : Unit = {
      System.err.println("ERROR/FATAL: Nonspecific exception " + f.toString)
      f.printStackTrace(System.err)
  }


  /** Prints warnings. */
  private def printWarns(warns : Iterable[Trace]) : Unit = {
    warns.foreach({
        case TrDuplicateDeclaration(f, n, s1, s2) ⇒
          System.err.println(f + ": Duplicate declaration of " + n)
      })
  }


  /** Prints a usage and exists. */
  private def printUsageAndExit() : Unit = {
    System.out.println("Usage: java -jar jssample.jar [out-file-name] [input-dir]+")
    System.exit(1)
  }
}
