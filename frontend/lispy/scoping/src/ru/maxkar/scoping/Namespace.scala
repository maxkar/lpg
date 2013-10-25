package ru.maxkar.scoping

import scala.collection.mutable.HashMap

/** Basic trait for namespaces.
 * @param T elements type
 */
trait Namespace[T] {
  /** Lookups set of elements in the namespace. */
  def lookup(key : String) : Set[T]
}


/** Namespace accessors. */
object Namespace {
  /** Creates a new namespace using a provided map as resolution cache. */
  def fromMap[T](input : Map[String, T])
      : Namespace[T] =
    new Namespace[T] {
      override def lookup(key : String) : Set[T] =
        input.get(key) match {
          case None ⇒ Set.empty
          case Some(x) ⇒ Set(x)
        }
    }


  /** Collects a namespace providers into one. */
  def collect[T](items : Seq[Namespace[T]]) : Namespace[T] =
    new Namespace[T] {
      /** Resolution cache. */
      private val cache = new HashMap[String, Set[T]]

      override def lookup(key : String) : Set[T] =
        cache.getOrElseUpdate(key,
          items.foldLeft(Set.empty[T])((s, n) ⇒ s ++ n.lookup(key)))
    }


  /** Provides a subnamespace. Lookup parent namespaces only
   * when child namespace does not find an item.*/
  def subspace[T](parent : Namespace[T], child : Namespace[T])
      : Namespace[T] =
    new Namespace[T] {
      /** Resolution cache. */
      private val cache = new HashMap[String, Set[T]]

      override def lookup(key : String) : Set[T] =
        cache.getOrElseUpdate(key, innerLookup(key))

      /** Lookups an item. */
      private def innerLookup(key : String) : Set[T] =  {
        val g1 = child.lookup(key)
        if (g1.isEmpty)
          parent.lookup(key)
        else
          g1
      }
    }
}
