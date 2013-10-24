package ru.maxkar.jssample

import ru.maxkar.lispy.Attribute

import ru.maxkar.hunk.Hunk._

/** Application runner class. */
final object Runner {
  /** Entry point. */
  def main(args : Array[String]) {
    if (args.length < 2)
      printUsageAndExit();

    val items = stage0.Processor.process(args.tail)

    val (s0fails, s0succs) = awaitSplit(items)

    printFailsAndExit(s0fails)
    s0succs.foreach(x ⇒ printWarns(x._2))

    System.out.println("Results:")
    s0succs.foreach(x ⇒ System.out.println(x._1))
  }

  /** Prints fails and exits. Exists only when at least on
   * failure exists.
   */
  private def printFailsAndExit(x : Seq[(Throwable, _)]) : Unit = {
    if (x.isEmpty)
      return
    x.foreach(f ⇒ printFail(f._1))
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
  private def printWarns(warns : Iterable[_]) : Unit = {
  }


  /** Prints a usage and exists. */
  private def printUsageAndExit() : Unit = {
    System.out.println("Usage: java -jar jssample.jar [out-file-name] [input-dir]+")
    System.exit(1)
  }
}
