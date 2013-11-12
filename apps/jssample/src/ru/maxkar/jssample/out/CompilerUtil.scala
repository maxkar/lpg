package ru.maxkar.jssample.out

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser.Input
import ru.maxkar.lispy.parser.TextPosition

/** Compiler utilities. */
private[out] object CompilerUtil {

  /** Calculates a location of the expression. */
  def locOf(x : SExpression[BaseItem]) : TextPosition = {
    val slist = x.atts.allValues(Input.textPosition)
    if (slist.isEmpty)
      null
    else
      slist.iterator.next
  }

}
