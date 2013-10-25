package ru.maxkar.jssample.ns

import java.math._

/** New lookup node. */
abstract sealed class LookupNode


/** Reference to an item. */
abstract sealed class Reference extends LookupNode


/** Reference to "variable declarator". */
final case object RefVar
    extends Reference

/** Reference to "constant declarator". */
final case object RefConst
    extends Reference

/** Reference to "function declarator". */
final case object RefDef
    extends Reference

/** Reference to a simple object. */
final case class RefNSMember(id : String, scope : Scope)
    extends Reference

/** Unresolved reference. */
final case class RefUnknown(id : String)
    extends Reference

/** String reference. */
final case class LkString(value : String)
  extends LookupNode


/** Integer value. */
final case class LkInteger(value : BigInteger)
  extends LookupNode


/** Floating-point number. */
final case class LkFloating(
  value : BigDecimal, expt : BigInteger)
    extends LookupNode
