package ru.maxkar.jssample.stages

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.lispy.parser.Input
import ru.maxkar.scoping.simple._
import ru.maxkar.lispy._
import ru.maxkar.jssample.att._

import ru.maxkar.collection.LazySeq



/**
 * S-Syntax analysis stage.
 * This stage checks s-expressions for well-formness and
 * provides list of s-syntax violations. This analyzer expects
 * that each S-Expression have at least location attribute.
 */
object SSyntax {

  /**
   * Input type.
   */
  type Input = SExpression[BaseItem]

  /**
   * Analyziz output type.
   */
  type Output = LazySeq[Error]

  /**
   * Generic location.
   */
  type Location = TextPosition

  /**
   * S-Syntax errors.
   */
  abstract sealed class Error

  /** Illegal (empty) expression. */
  final case class EmptyExpression(
      val location : Location)
    extends Error


  // ANALYZERS //
  def analyzeExpression(input : Input) : Output =
    input match {
      case SLeaf(BaseInteger(_), _) ⇒ LazySeq.empty
      case SLeaf(BaseFloating(_, _), _) ⇒ LazySeq.empty
      case SLeaf(BaseString(_), _) ⇒ LazySeq.empty
      case SLeaf(BaseId(_), _) ⇒ LazySeq.empty
      case SList(Seq(), _) ⇒
        LazySeq(EmptyExpression(locationOf(input)))
      case SList(items, _) ⇒
        LazySeq.sum(items map analyzeExpression : _*)
    }


  // UTIL //

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
