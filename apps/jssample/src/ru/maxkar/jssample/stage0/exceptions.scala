package ru.maxkar.jssample.stage0

import java.io._


/** Stage-0 failure. */
abstract sealed class Failure(msg : String, cause : Throwable) extends Exception(msg, cause)


/** File input failure. */
final case class ReadFailure(f : File, cause : IOException)
  extends Failure("Failed to read file " + f, cause)
