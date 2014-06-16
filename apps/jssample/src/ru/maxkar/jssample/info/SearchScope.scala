package ru.maxkar.jssample.info

import ru.maxkar.collection.LazySeq


import SearchScope._

/**
 * Search scope implementation.
 */
abstract class SearchScope private() {
  /**
   * @param useAutoScope flag indicating that search should be
   *   performed using the automatic search scope.
   */
  protected[info] val context : LazySeq[SearchStep]

  /**
   * @param context list of search contexts to use.
   */
  protected[info] val useAutoScope : Boolean

  /**
   * Nests this context into another context.
   */
  protected[info] def nest(outer : SearchStep) : SearchScope
}



/**
 * Module for the variable search context definition.
 */
object SearchScope {
  /**
   * Explicit search scope.
   */
  private class ExplicitContext(ref : NamespaceRef.Ref) extends SearchScope {
    override protected[info] val context : LazySeq[SearchStep] = LazySeq(Seq(ref))
    override protected[info] val useAutoScope = false
    override protected[info] def nest(outer : SearchStep) : SearchScope = this
  }


  /**
   * Automatic search type.
   */
  private class AutoContext(
        override protected[info] val context : LazySeq[SearchStep])
      extends SearchScope {
    override protected[info] val useAutoScope = true
    override protected[info] def nest(outer : SearchStep) : SearchScope =
      new AutoContext(context :+ outer)
  }


  /**
   * Search step.
   */
  type SearchStep = Seq[NamespaceRef.Ref]


  /**
   * Search scope type.
   */
  type T = SearchScope


  /**
   * Creates a new explicit scope. Variable in that scope
   * is resolved only in the given namespace.
   */
  def explicit(ref : NamespaceRef.Ref) : T =
    new ExplicitContext(ref)


  /**
   * Creates a new automatic search context. Search is
   * performed in the automatic context and possible in
   * the explicit extension contexts.
   */
  def auto(scopes : Seq[NamespaceRef.Ref] = Seq.empty) : T =
    new AutoContext(LazySeq(scopes))


  /**
   * Nests search context into another one.
   */
  def nest(item : T, outer : Seq[NamespaceRef.Ref]) : T =
    item.nest(outer)


  /**
   * Resolves a variable using local context.
   */
  def resolveLocal[I, R](
        resolver : I ⇒ Option[R],
        scope : T,
        item : I)
      : Option[R] =
    if (scope.useAutoScope)
      resolver(item)
    else
      None


  /**
   * Resolves items using a nested scopes.
   */
  def resolveAuto[I, R](
        localResolver : I ⇒ Option[R],
        resolver : (SearchStep, I) ⇒ Option[R],
        scope : T,
        item : I) :
      Option[R] = {
    resolveLocal(localResolver, scope, item) match {
      case Some(a) ⇒ Some(a)
      case None ⇒
        val itr = scope.context.iterator
        while (itr.hasNext)
          resolver(itr.next, item) match {
            case Some(a) ⇒ return Some(a)
            case None ⇒ ()
          }
        None
    }
  }


  /**
   * Resolves using a simple scope resolver.
   * Returns right(value) if resolution is successfull.
   * Returns left[] if resolution was not able to find anything.
   * Returns left(Seq[NamespaceRef.Ref]) if value was found in
   *   several namespaces.
   */
  def resolveAutoNS[I, R](
        localResolver : I ⇒ Option[R],
        resolver : (NamespaceRef.Ref, I) ⇒ Option[R],
        scope : T,
        item : I) :
      Either[Seq[NamespaceRef.Ref], R] = {

    def compRes(context : NamespaceRef.Ref)
        : Option[(NamespaceRef.Ref, R)] =
      resolver(context, item).map((context, _))

    def localRes(step : SearchStep, item : I)
        : Option[Either[Seq[NamespaceRef.Ref], R]] = {
      step.flatMap(compRes) match {
        case Seq() ⇒ None
        case Seq((_, a)) ⇒ Some(Right(a))
        case lst ⇒ Some(Left(lst.map(_._1)))
      }
    }

    def altLocal(item : I) : Option[Either[Nothing, R]] =
      localResolver(item).map(Right(_))


    resolveAuto(altLocal, localRes, scope, item) match {
      case None ⇒ Left(Seq.empty)
      case Some(x) ⇒ x
    }

  }

}
