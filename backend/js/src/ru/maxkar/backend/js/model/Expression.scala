package ru.maxkar.backend.js.model

import Model._

/**
 * Base class for all expressions.
 */
class Expression(
      final private [model] val base : E,
      final private [model] val simpleMemberAccessor : Option[String] = None
    )
