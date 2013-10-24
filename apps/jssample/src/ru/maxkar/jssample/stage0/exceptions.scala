package ru.maxkar.jssample.stage0

import java.io._

import ru.maxkar.lispy.front.stage0._

/** Stage-0 failure. */
abstract sealed class Failure(msg : String, cause : Throwable) extends Exception(msg, cause) {
  /** Failed file. */
  val file : File
}


/** File input failure. */
final case class ReadFailure(file : File, cause : IOException)
  extends Failure("Failed to read file " + file, cause)


/** S-expression format failure. */
final case class SFormatFailure(file : File, cause : SFormatException)
  extends Failure("Mailformed s-expressoin : " + cause, cause)
