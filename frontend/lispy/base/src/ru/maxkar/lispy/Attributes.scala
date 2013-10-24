package ru.maxkar.lispy


/** Set of attributes for a given entity. */
final class Attributes private(private val map : Map[Attribute[_], Set[Any]]) {

  /** Merges one element. */
  private def merge1(map : Map[Attribute[_], Set[Any]],
      pair : (Attribute[_], Set[Any])) : Map[Attribute[_], Set[Any]] =
    map.updated(pair._1, map.getOrElse(pair._1, Set.empty) ++ pair._2)


  /** Adds two attributes sets. */
  def ++(other : Attributes) : Attributes =
    new Attributes(other.map.foldLeft(map)(merge1))


  /** Adds one item. */
  def +[T](key : Attribute[T], value : T) : Attributes =
    new Attributes(map.updated(key, map.getOrElse(key, Set.empty) + value))


  /** Retreives all values for the attribute. */
  def allValues[T](key : Attribute[T]) : Set[T] =
    map.getOrElse(key, Set.empty).map(x ⇒ x.asInstanceOf[T])


  override def equals(other : Any) : Boolean = {
    if (!other.isInstanceOf[Attributes])
      false
    else
      other.asInstanceOf[Attributes].map == map
  }


  override def hashCode() : Int = map.hashCode


  override def toString() : String = "Attributes[" + map + "]"
}


/** Attributes accessors. */
object Attributes {
  /** Empty attributes (none) */
  val empty : Attributes = new Attributes(Map.empty)


  /** Creates a singleton attribute from this attributes. */
  def singleton[T](key : Attribute[T], value : T) : Attributes =
    new Attributes(Map(key → Set(value)))
}
