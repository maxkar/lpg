package ru.maxkar.jssample

import java.util.concurrent._

import ru.maxkar.lispy.Attribute
import ru.maxkar.hunk.Hunk._


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

    val p1 = new stage1.Processor

    val (s1fails, s1succs) = awaitSplit(
      s0succs.map(a ⇒ p1.proc _ <**> succHunk(a)))

    printFailsAndExit(s1fails)
    s1succs.foreach(x ⇒ stage1.Msg.printErrors(
      System.err, x.source, x.anamnesis))

    val s2result = new stage2.Processor().process(s1succs)
    printFails2AndExit(s2result.anamnesis)

    val p3 = new stage3.Processor(s2result.mods)
    val p3result = s1succs.map(p3.process)
    val (s3fails, s3succ) = awaitSplit(p3result)
    printFailsAndExit(s3fails)

    System.out.println(s3succ)

    executor.shutdownNow
  }


  def printFails2AndExit(x : stage2.Anamnesis2) : Unit = {
    if (x.moddecls.isEmpty)
      return
    x.moddecls.foreach(md ⇒ {
        System.err.println("ERROR : " + md.second + " : Module " +
          md.name.mkString(".") + " is already declared in " + md.first)
    })
    System.exit(2)
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
