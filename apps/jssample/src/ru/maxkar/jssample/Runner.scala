package ru.maxkar.jssample

import ru.maxkar.lispy.front.Attribute

/** Application runner class. */
final object Runner {
  /** Entry point. */
  def main(args : Array[String]) {
    if (args.length < 2)
      printUsageAndExit();
  }


  /** Prints a usage and exists. */
  private def printUsageAndExit() : Unit = {
    System.out.println("Usage: java -jar jssample.jar [out-file-name] [input-dir]+")
    System.exit(1)
  }
}
