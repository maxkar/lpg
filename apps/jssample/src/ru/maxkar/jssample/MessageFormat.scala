package ru.maxkar.jssample

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.Input
import ru.maxkar.lispy.parser.TextPosition

import java.io._

/** Message formatting utilities. */
final object MessageFormat {


  def formatLocation(x : TextPosition) : String =
    "(" + x.line + "," + x.column + ")"


  /** Formats a source location. */
  def formatLocation(atts : Attributes) : String = {
    val aset = atts.allValues(Input.textPosition)
    if (aset.size != 1)
      return "<unknown/unsupported location>"
    formatLocation(aset.head)
  }


  /** Formats an error. */
  def err(file : File, atts : Attributes, msg : String) : String =
    "ERROR: " + file + " " + formatLocation(atts) + ": " + msg

  /** Formats an error. */
  def err(file : File, atts : TextPosition, msg : String) : String =
    "ERROR: " + file + " " + formatLocation(atts) + ": " + msg
}
