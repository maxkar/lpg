package ru.maxkar.jssample

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.Input

import java.io._

/** Message formatting utilities. */
final object MessageFormat {


  /** Formats a source location. */
  def formatLocation(atts : Attributes) : String = {
    val aset = atts.allValues(Input.textPosition)
    if (aset.size != 1)
      return "<unknown/unsupported location>"
    val x = aset.head
    "(" + x.line + "," + x.column + ")"
  }


  /** Formats an error. */
  def err(file : File, atts : Attributes, msg : String) : String =
    "ERROR: " + file + " " + formatLocation(atts) + ": " + msg

}
