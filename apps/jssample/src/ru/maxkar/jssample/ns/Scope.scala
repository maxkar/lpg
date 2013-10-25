package ru.maxkar.jssample.ns

import ru.maxkar.lispy._
import ru.maxkar.alias.collection._

/** Scoping host/namespace ID. */
abstract sealed class Scope

/** Extensible scope host, may contain nested scopes. */
abstract sealed class ExtensibleScope extends Scope

/** System names. */
final case object SystemScope extends Scope

/** User module scope. */
final case class ModuleScope(id : Seq[String])
    extends ExtensibleScope

/** Nested scope. */
final case class Subscope(parent : ExtensibleScope,
    location : Attributes, declNames : JSet[String]) extends ExtensibleScope
