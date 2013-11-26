package ru.maxkar.jssample

import java.util.concurrent._

import ru.maxkar.lispy.Attribute

import ru.maxkar.jssample.ns._

import ru.maxkar.jssample.msg._
import ru.maxkar.jssample.doc.DocGen

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
    if (args.length < 4)
      printUsageAndExit();

    val platf = readPlatform(new File(args(2)))

    implicit val executor =
      Executors.newFixedThreadPool(
        Runtime.getRuntime().availableProcessors)

    val boot = (new stage0.Processor).process(args.drop(3))

    val s0succs = mowait(boot)

    val s1runs = s0succs.map(x ⇒  exec {
        (x._1, out.Premodule.precompile(x._1, x._2.path, x._2.source, x._2.body))})

    val s1succs = mwait(s1runs)

    val modmap = createModsScope(s1succs.map(x ⇒  x._2))
    val globsMap = createGlobalsMap(platf, s1succs.map(_._2))


    val s2runs = s1succs.map(x ⇒  exec {
        (x._1, (x._2.id, x._2.module, x._2.after, x._2.compile(platf.scope, modmap, x._1)))
      })

    val s2succs = mwait(s2runs)

    val resf = collectjs(globsMap, s2succs.map(_._2))

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

    val docDir = new File(args(1))
    docDir.mkdirs()

    val moddoc : Seq[String] = s1succs.map(x ⇒
      DocGen.writeDoc(docDir, x._2.id, x._2.doc, x._2.varDoc, x._2.funDoc))
    DocGen.writeMeta(moddoc, docDir)
  }


  /** Reads a platform file. */
  def readPlatform(file : File) : platform.Platform = {
    try {
      platform.Platform.fromFile(file)
    } catch {
      case e : Throwable ⇒
        System.err.println("ERROR: Failed to read platform file : " + e.getMessage)
        e.printStackTrace(System.err)
        System.exit(2)
        null
      }
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


  /** Prints a cyclic dependency. */
  private def printCycle(items : java.util.Collection[String], key : String) : Unit = {
    val itr = items.iterator
    while (itr.next != key) {}

    System.err.print("ERROR: Cyclic module order: ")
    System.err.print(key)
    System.err.print(" → ")
    while (itr.hasNext) {
      System.err.print(itr.next)
      System.err.print(" → ")
    }
    System.err.println(key)
  }


  /** Performs a topological sort of items. */
  private def modSort[T](items : Seq[(Seq[String], File, Set[ModuleRef], T)]) : Seq[T] = {
    val res = new ArrayBuffer[T]
    val stack = new java.util.LinkedHashSet[String]
    val found = new java.util.HashSet[String]

    val imap = items.map(x ⇒ (x._1.mkString("."), (x._2, x._3, x._4))).toMap
    var fails = false

    def proc(key : String, item : (File, Set[ModuleRef], T)) : Unit = {
      if (!found.contains(key)) {
        if (!stack.add(key)) {
            fails = true
            printCycle(stack, key)
            found.add(key)
        } else {
          for (dep ← item._2)
            imap.get(dep.name) match {
              case None ⇒ System.err.println(
                MessageFormat.err(item._1, dep.loc, " Reference to unexisting module " + dep.name))
                fails = true
              case Some(n) ⇒
                proc(dep.name, n)
            }

          res += item._3
          stack.remove(key)
          found.add(key)
        }
      }
    }

    for ((k, v) ← imap)
      proc(k, v)

    if (fails)
      System.exit(5)

    res
  }


  /** Collects a js object. */
  private def collectjs(
        globs : Map[AnyRef, String],
        items : Seq[(Seq[String], File, Set[ModuleRef], (Set[out.Symbol], Seq[(out.Symbol, FunctionBody)], Seq[Statement]))])
      : JSFile = {

    var a = Set.empty[AnyRef]
    var b = Seq.empty[(AnyRef, FunctionBody)]
    var c = Seq.empty[Statement]

    modSort(items).foreach(x ⇒  {
        a ++= x._1
        b ++= x._2
        c ++= x._3
      })

    Model.file(globs.toSeq, a.toSeq, b, c)
  }

  /** Creates a modules scope. */
  private def createModsScope(scopes : Seq[out.Premodule]) : Scope[String, out.Premodule] = {

    var gsb = ScopeBuilder.collecting[String, out.Premodule]

    scopes.foreach(x ⇒  gsb.offer(x.id.mkString("."), x))

    val errs = gsb.duplicates
    for ((n, i1, i2) ← errs)
      System.err.println("ERROR: " + i2.module +
        "Duplicate definition of module " + n +
        ", previous declaration at\n  " +
        i1.module)

    if (!errs.isEmpty)
      System.exit(3)

    gsb.scope
  }


  /** Creates a global scope. */
  private def createGlobalsMap(platf : platform.Platform, scopes : Seq[out.Premodule])
      : Map[AnyRef, String] = {

    var gsb = ScopeBuilder.collecting[String, out.Symbol]
    var rev = new scala.collection.mutable.HashMap[AnyRef, String]

    for ((k, v) ← platf.items) {
      gsb.offer(k, v)
      rev.put(v, k)
    }

    for (s ← scopes)
      for (e ← s.globals)
        gsb.offer(e._1, e._2)

    for (s ← scopes)
      for (e ← s.globals)
        rev.put(e._2, e._1)

    val errs = gsb.duplicates

    for ((n, i1, i2) ← errs)
      System.err.println(MessageFormat.err(i2.declaration.file,
        i2.declaration.offset, "Duplicate definition of global " + n +
        ", previous declaration at\n  " +
        i1.declaration.file + ":" +
        MessageFormat.formatLocation(i1.declaration.offset)))

    if (!errs.isEmpty)
      System.exit(3)

    rev.toMap
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
    System.out.println("Usage: java -jar jssample.jar <out-file-name> <doc-dir> <platform-file> <input-dir>+")
    System.exit(1)
  }
}
