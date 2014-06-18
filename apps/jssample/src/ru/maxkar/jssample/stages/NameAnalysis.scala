package ru.maxkar.jssample.stages

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input
import ru.maxkar.scoping.simple._
import ru.maxkar.lispy._
import ru.maxkar.jssample.att._

import ru.maxkar.jssample.{ info ⇒ i }

/**
 * Name analysis stage. This stage is aimed to provide following info:
 * <ul>
 *   <li> All referenced namespaces.
 * </ul>
 */
object NameAnalysis {
  /**
   * Analysis result type.
   */
  type Result = i.Compound.T

  /**
   * Generic location.
   */
  type Location = TextPosition


  /**
   * Input type.
   */
  type Input = SExpression[BaseItem]


  /**
   * Analyses the input as an expression and provides
   * an analysis result.
   */
  def analyzeExpression(input : Input) : Result =
    input match {
      case SLeaf(BaseInteger(_), _) ⇒ i.Compound.literal
      case SLeaf(BaseFloating(_, _), _) ⇒ i.Compound.literal
      case SLeaf(BaseString(_), _) ⇒ i.Compound.literal
      case SLeaf(BaseId(id), _) ⇒ i.Compound.name()
      case SList(items, atts) ⇒
        i.Compound.unit
    }


  // UTILITIES //



  /**
   + Location accessor. This method pretends that location
   * is set on every input element.
   */
  private def locationOf(item : Input) : Location = {
    val locs = item.atts.allValues(Input.textPosition)
    if (locs.size != 1)
      throw new IllegalArgumentException("Mailformed location attribute in " + item);
    locs.head
  }

}
