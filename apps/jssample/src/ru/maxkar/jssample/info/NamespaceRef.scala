package ru.maxkar.jssample.info

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.comp.vars.Reference
import ru.maxkar.collection.LazySeq

/**
 * Information about referenced namespaces.
 * This class is very similar to ocaml module.
 */
object NamespaceRef {
  /**
   * Namespace reference location.
   */
  type Location = TextPosition


  /**
   * Descriptor (name) type.
   */
  type Name = String


  /**
   * Namespace reference type.
   */
  type Ref = Reference[Name, Location]


  /**
   * Namespace reference statistics.
   */
  type Stats = LazySeq[Ref]


  /**
   * Empty statistics.
   */
  val empty : Stats = LazySeq.empty


  /**
   * Creates a new reference.
   */
  def ref(name : Name, location : Location) : Ref =
    Reference(name, location)


  /**
   * Creates statistics for the explicit reference.
   */
  def explicit(ref : Ref) : Stats =
    LazySeq(ref)


  /**
   * Creates a multi-search request context.
   */
  def multi(refs : Seq[Ref]) : Stats =
    LazySeq(refs : _*)


  /**
   * Combines several stats into one.
   */
  def combine(items : Seq[Stats]) : Stats =
    LazySeq.sum(items : _*)


  /**
   * Generic combinator for the tree node.
   * Combines statistics from child nodes and extends it
   * using node search scope.
   * @param extension search scope extension.
   * @param subnodes items in the subnodes.
   */
  def astNodeFast(
        extension : Seq[Ref] = Seq.empty,
        subnodes : Seq[Stats] = Seq.empty)
      : Stats =
    multi(extension) ++ LazySeq.sum(subnodes : _*)


  /**
   * Generic combinator for the AST node.
   * Combines statistics from child nodes and extends it
   * using node search scope.
   * @param T AST node type.
   * @param extension search scope extension.
   * @param subnodes items in the subnodes.
   * @param extractor statistics extractor for the given node.
   */
   def astNode[T](
        extension : Seq[Ref] = Seq.empty,
        subnodes : Seq[T] = Seq.empty,
        extractor : T ⇒ Stats)
      : Stats =
    astNodeFast(extension, subnodes.map(extractor))
}
