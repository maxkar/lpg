package ru.maxkar.backend.js.model

import ru.maxkar.backend.js.out.CompactContext

/**
 * Base class for all expressions.
 * @param priority expression priority. Items with a lower value binds earlier.
 * @param isCommaSafe flag, indicating, that expression can safely be written
 *   after the comma.
 * @param isMinusSafe indicates that this expression can be written after minus sign.
 */
class Expression(
      final private [model] val priority : Int,
      final private [model] val writer : CompactContext ⇒ Unit,
      final private [model] val isCommaSafe : Boolean = true,
      final private [model] val isMinusSafe : Boolean = true,
      final private [model] val canStartStatement : Boolean = true,
      final private [model] val simpleMemberAccessor : Option[String] = None
    )
