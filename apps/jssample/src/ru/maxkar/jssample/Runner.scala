package ru.maxkar.jssample

import java.util.concurrent._

import ru.maxkar.lispy.Attribute
import ru.maxkar.hunk.Hunk._

import ru.maxkar.scoping.simple._
import ru.maxkar.backend.js.model._

import out.ToplevelItem

import scala.collection.JavaConversions._

import java.io._


/** Application runner class. */
final object Runner {

  def main(args : Array[String]) {
    if (args.length < 2)
      printUsageAndExit();

    implicit val executor =
      Executors.newFixedThreadPool(
        Runtime.getRuntime().availableProcessors)

    val items = (new stage0.Processor).process(args.tail)

    val (s0fails, s0succs) = awaitSplit(items)

    printFailsAndExit(s0fails)

    val s1runs = s0succs.map(x ⇒  exec {
        out.Premodule.precompile(x.source, x.body)})

    val (s1fails, s1succs) = awaitSplit(s1runs)

    printFailsAndExit(s1fails)

    val msgs1 = s1succs.map(_._2).flatten
    msgs1.foreach(out.Message.printMsg(System.err, _))
    if (!msgs1.isEmpty)
      System.exit(2)

    val globScope = createGlobScope(s1succs.map(_._1))

    val s2runs = s1succs.map(x ⇒  exec {
        x._1.compile(globScope)
      })

    val (s2fails, s2succs) = awaitSplit(s2runs)

    printFailsAndExit(s2fails)
    val msgs2 = s2succs.map(_._2).flatten
    msgs2.foreach(out.Message.printMsg(System.err, _))
    if (!msgs2.isEmpty)
      System.exit(4)

    val resf = collectjs(s2succs.map(_._1))

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

    Model.file(Seq.empty, a.toSeq, b, c)
  }


  /** Creates a global scope. */
  private def createGlobScope(scopes : Seq[out.Premodule]) : Scope[String, ToplevelItem] = {
    var gsb = new ScopeBuilder[String, ToplevelItem]

    for (s ← scopes)
      for (e ← s.defKeys.entrySet)
        gsb.offer(e.getKey, e.getValue)

    val errs = gsb.duplicates

    for ((n, i1, i2) ← errs)
      System.err.println(MessageFormat.err(i2.declarationHost.file,
        i2.declarationHost.offset, "Duplicate definition of global " + n +
        ", previous declaration at\n  " +
        i1.declarationHost.file + ":" +
        MessageFormat.formatLocation(i1.declarationHost.offset)))

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
    f match {
      case x : stage0.Failure ⇒ stage0.Msg.printException(System.err, x)
      case _ ⇒
        System.err.println("ERROR/FATAL: Nonspecific exception " + f.toString)
        f.printStackTrace
    }
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
