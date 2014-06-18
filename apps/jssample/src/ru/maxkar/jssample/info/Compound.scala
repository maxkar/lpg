package ru.maxkar.jssample.info

import ru.maxkar.lispy.parser.TextPosition


/**
 * Compound statistics. This statistics is usually fused from the
 * simplier components on the ast.
 */
final class Compound private(
    _referencedNamespaces : ns.Referenced.T) {

  /**
   * Namespaces referenced in the compound.
   */
  def referencedNamespaces() : ns.Referenced.T =
    _referencedNamespaces
}



/**
 * Compound statistics module.
 */
object Compound {

  // TYPES //

  /**
   * Compound location type.
   */
  type Location = TextPosition


  /**
   * Statistics type.
   */
  type T = Compound


  // EXTRACTORS //


  /**
   * Provides list of all the namespaces referenced by
   * the statistics.
   */
  def referencedNamespaces(stat : T) : ns.Referenced.T =
    stat.referencedNamespaces()


  // FACTORIES : SIMPLE //


  /**
   * Statistics for Unit type reference.
   */
  val unit : T = new Compound(ns.Referenced.empty)


  /**
   * Statistics for literals in the source.
   */
  val literal : T = new Compound(ns.Referenced.empty)


  /**
   * Creates statistics for the automatic (non-ns) name reference.
   */
  def name() : T =
    new Compound(ns.Referenced.empty)


  /**
   * Creates statistics for a name with explicit namespace
   * reference.
   * @param nsName name of the referenced namespace.
   * @param nsLocation name of the referenced location.
   */
  def nameInExplicitNamespace(
        nsName : ns.Referenced.Name,
        nsLocation : ns.Referenced.Location)
      : T =
    new Compound(ns.Referenced(nsName, nsLocation))


  /**
   * Creates statistics for a name inside specific search context.
   */
  def nameInSearhNamespaces(
        namespaces : Seq[ns.Referenced.Ref])
      : T =
    new Compound(ns.Referenced(namespaces : _*))


  // FACTORIES : AST //


  /**
   * Creates an application node.
   */
  def applicationFast(
        app : Seq[T],
        extraNamespaces : Seq[ns.Referenced.Ref] = Seq.empty)
      : T =
    new Compound(
      ns.Referenced.astNode(
        extraNamespaces,
        app,
        referencedNamespaces))


   /**
    * AST-like application.
    */
   def applicationAstNode[N](
        app : Seq[N],
        extraNamespaces : Seq[ns.Referenced.Ref] = Seq.empty,
        extractor : N ⇒ T)
      : T =
    applicationFast(app.map(extractor), extraNamespaces)
}
