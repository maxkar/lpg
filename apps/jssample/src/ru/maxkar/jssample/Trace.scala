package ru.maxkar.jssample

import java.io._
import ru.maxkar.lispy._

/** Trace messages. */
abstract sealed class Trace


/** "Duplicate declaration" message. */
final case class TrDuplicateDeclaration(
  file : File, name : String, first : Attributes, next : Attributes)
  extends Trace
