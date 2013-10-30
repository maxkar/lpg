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

    val s1runs = s0succs.map(x ⇒  exec {
        out.Premodule.precompile(x.source, x.body)})

    val (s1fails, s1succs) = awaitSplit(s1runs)

    printFailsAndExit(s1fails)

    val msgs1 = s1succs.map(_._2).flatten
    msgs1.foreach(out.Message.printMsg(System.err, _))
    if (!msgs1.isEmpty)
      System.exit(2)

    executor.shutdownNow
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
