package ru.maxkar.jssample.info

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.comp.vars.Declaration
import ru.maxkar.collection.LazySeq

/**
 * Name declaration module.
 */
object NameDeclaration {
  /**
   * Name declaration location.
   */
  type Location = TextPosition

  /**
   * Name of the declaration.
   */
  type Name = String

  /**
   * Declaration type.
   */
  type Decl = Declaration[Name, Location]

  /**
   * Nested declaration statistics.
   */
  type Decls = LazySeq[Decl]

  /**
   * Creates a new name declaration.
   */
  def make(name : String, location : Location) : Decls =
    LazySeq(Declaration(name, location))

  /**
   * Combines declaration in an ast-like manner.
   */
  def astNodeFast(
        locals : Decls,
        subs : Seq[Decls])
      : Decls =
    locals ++ LazySeq.sum(subs : _*)

  /**
   * Generic ast node combinator.
   */
  def astNode[T](
        locals : Seq[Decl] = Seq.empty,
        subs : Seq[T] = Seq.empty,
        extractor : T ⇒ Decls)
      : Decls =
    astNodeFast(LazySeq(locals : _*), subs.map(extractor))
}
