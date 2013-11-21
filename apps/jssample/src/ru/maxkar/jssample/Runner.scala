package ru.maxkar.jssample

import java.util.concurrent._

import ru.maxkar.lispy.Attribute

import ru.maxkar.jssample.msg._
import ru.maxkar.jssample.doc._

import ru.maxkar.hunk.Hunk._
import ru.maxkar.hunk.Hunk

import ru.maxkar.scoping.simple._
import ru.maxkar.backend.js.model._

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

import java.io._
import com.coverity.security.Escape._


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
        (x._1, x._2.compile(platf.scope, modmap, x._1))
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

    val moddoc : Seq[String] = s1succs.flatMap(x ⇒  writeDoc(docDir, x._2))
    writeDocCsss(docDir)
    writeMeta(moddoc, docDir)
  }


  /** Writes a meta-documentation. */
  private def writeMeta(moddoc : Seq[String], base : File) : Unit = {
    if (moddoc.isEmpty)
      return

    val fs = new FileOutputStream(new File(base, "index.html"))
    try {
      val pn = new OutputStreamWriter(new BufferedOutputStream(fs), "UTF-8")
        pn.write("<!DOCTYPE html><html><head><meta charset=\"utf-8\">")
        pn.write("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"><title>JSLPG documentation")
        pn.write("</title></head><body class=\"framedoc\">")
        pn.write("<div class=\"pkglist\"><h1>Packages</h1>")
        for (x ← moddoc) {
          pn.write("<div><a target=\"moditem\" href=\"_")
          pn.write(html(x))
          pn.write(".html\">")
          pn.write(html(x))
          pn.write("</a></div>")
        }
        pn.write("</div>")
        pn.write("<div class=\"modframe\"><iframe name=\"moditem\" seamless=\"true\"/></div>")
        pn.write("</body></html>")
      try {
      } finally {
        pn.close
      }
    } finally {
      fs.close
    }
  }


  /** Writes a documentation css. */
  private def writeDocCsss(base : File) : Unit = {
    val fs = new FileOutputStream(new File(base, "style.css"))
    try {
      fs.write(DocStyle.STYLE.getBytes("UTF-8"))
    } finally {
      fs.close
    }
  }


  /** Writes a single documentation file. */
  private def writeDoc(base : File, mod : out.Premodule) : Option[String] = {
    if (!mod.doc.isDefined && mod.varDoc.isEmpty && mod.funDoc.isEmpty)
      return None

    val modName = mod.id.mkString(".")

    val fs = new FileOutputStream(new File(base, "_" + modName + ".html"))
    try {
      val pn = new OutputStreamWriter(new BufferedOutputStream(fs), "UTF-8")
      try {
        pn.write("<!DOCTYPE html><html><head><meta charset=\"utf-8\">")
        pn.write("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"><title>Module ")
        pn.write(htmlText(modName))
        pn.write("</title></head><body>")
        pn.write("<header class=\"modname\"><h1>Module ")
        pn.write(htmlText(modName))
        pn.write("</h1></header>")

        mod.doc match {
          case None ⇒  ()
          case Some(x) ⇒
            pn.write("<div class=\"docarea\">")
            DocBody.write(x, pn)
            pn.write("</div>")
        }

        if (!mod.varDoc.isEmpty) {
          pn.write("<div class=\"sect\"><header class=\"sectheader\"><h2>Variables</h2></header>")
          pn.write("<table><thead><tr><td>Access</td><td>Name</td><td>Exported</td><td class=\"descr\">Description</td></tr></thead>")

          mod.varDoc.foreach(docVar(_, pn))

          pn.write("</table></div>")
        }


        if (!mod.funDoc.isEmpty) {
          pn.write("<div class=\"sect\"><header class=\"sectheader\"><h2>Functions</h2></header>")
          pn.write("<table><thead><tr><td>Access</td><td>Name</td><td>Exported</td><td class=\"descr\">Description</td></tr></thead>")

          mod.funDoc.foreach(docFun(_, pn))

          pn.write("</table></div>")
        }

        pn.write("</body></html>")
      } finally {
        pn.close
      }
    } finally {
      fs.close
    }

    Some(modName)
  }


  /** Documents a function. */
  private def docFun(v : FunDoc, w : Writer) : Unit = {
    w.write("<tr><td>")
    w.write(if (v.isPublic) "public" else "private")
    w.write("</td><td>")
    w.write(htmlText(v.name))
    w.write("</td><td>")
    v.globalName match {
      case None ⇒  ()
      case Some(x) ⇒ w.write(htmlText(x))
    }
    w.write("</td><td>")
    DocBody.write(v.contract, w)

    if (!v.args.isEmpty) {
      w.write("<div class=\"fnsection\">Arguments:</div><div class=\"fndata\"><table class=\"alist\">")
      w.write("<thead><tr><td>Name</td><td class=\"descr\">Description</td></tr></thead>")
      for (a ← v.args) {
        w.write("<tr><td>")
        w.write(htmlText(a.name))
        w.write("</td><td>")
        DocBody.write(a.doc, w)
        w.write("</td></tr>")
      }
      w.write("</table></div>")
    }


    w.write("<div class=\"fnsection\">Returns:</div><div class=\"fndata\">")
    DocBody.write(v.ret, w)
    w.write("</div>")
    w.write("</td></tr>")
  }


  /** Documents a variable. */
  private def docVar(v : VarDoc, w : Writer) : Unit = {
    w.write("<tr><td>")
    w.write(if (v.isPublic) "public" else "private")
    w.write("</td><td>")
    w.write(htmlText(v.name))
    w.write("</td><td>")
    v.globalName match {
      case None ⇒  ()
      case Some(x) ⇒ w.write(htmlText(x))
    }
    w.write("</td><td>")
    DocBody.write(v.doc, w)
    w.write("</td></tr>")
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


  /** Collects a js object. */
  private def collectjs(
        globs : Map[AnyRef, String],
        items : Seq[(Set[out.Symbol], Seq[(out.Symbol, FunctionBody)], Seq[Statement])])
      : JSFile = {

    var a = Set.empty[AnyRef]
    var b = Seq.empty[(AnyRef, FunctionBody)]
    var c = Seq.empty[Statement]

    items.foreach(x ⇒  {
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
