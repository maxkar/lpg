package ru.maxkar.jssample.info.ns

import ru.maxkar.lispy.parser.TextPosition
import ru.maxkar.comp.vars.Reference
import ru.maxkar.collection.LazySeq


/**
 * Statistics for referenced namespaces.
 */
object Referenced {
  /**
   * Namespace reference location.
   */
  type Location = TextPosition


  /**
   * Descriptor (name) type.
   */
  type Name = String


  /**
   * Single element reference type.
   */
  type Ref = Reference[Name, Location]


  /**
   * Namespace reference statistics.
   */
  type T = LazySeq[Ref]


  /**
   * Empty statistics (no references).
   */
  val empty : T = LazySeq.empty


  /**
   * Creates a new reference.
   */
  def ref(name : Name, location : Location) : Ref =
    Reference(name, location)

  /**
   * Creates a new simple statistics.
   */
  def simple(name : Name, location : Location) : T =
    LazySeq(ref(name, location))


  /**
   * Creates a multiple-referenced stats.
   */
  def multi(refs : Seq[Ref]) : T =
    LazySeq(refs : _*)


  /**
   * Creates a new simple statistics.
   * Alias for #simple(Name, Location).
   */
  def apply(name : Name, location : Location) : T =
    simple(name, location)


  /**
   * Creates a multiple-referenced stats.
   * Convenience method similar to #multi(Seq[Ref])
   */
  def apply(refs : Ref*) : T =
    multi(refs)


  /**
   * Combines several stats into one.
   */
  def combine(items : Seq[T]) : T =
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
        subnodes : Seq[T] = Seq.empty)
      : T =
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
   def astNode[N](
        extension : Seq[Ref] = Seq.empty,
        subnodes : Seq[N] = Seq.empty,
        extractor : N ⇒ T)
      : T =
    astNodeFast(extension, subnodes.map(extractor))
}
