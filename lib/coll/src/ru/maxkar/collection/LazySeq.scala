package ru.maxkar.collection

/**
 * "Lazy" sequence. Unlike scala collection, this sequence does not create
 * an array or any other "fixed" represetnation. Instead this collection
 * keeps an initial structure and traverse it as needed.
 */
abstract sealed class LazySeq[+T] private()
    extends Iterable[T] {

  /** Adds another sequence to this. */
  def ++[T1 >: T](other : LazySeq[T1]) : LazySeq[T1] =
    if (other `eq` LazySeq.Empty)
      this
    else
      new LazySeq.SingleSum(this, other)

  /** Filters a sequence using a predicate. */
  override def filter(predicate : T ⇒ Boolean) : LazySeq[T] =
    new LazySeq.FilteredSeq(this, predicate)

  /** Maps a seqence. */
  def mapV[R](mapper : T ⇒ R) : LazySeq[R] =
    new LazySeq.MapSeq(this, mapper)

  /** Addition to right. */
  def :+[T1 >: T] (item : T1) : LazySeq[T1] =
    this ++ LazySeq.make(item)


  /** Addition to left. */
  def +:[T1 >: T] (item : T1) : LazySeq[T1] =
    LazySeq.make(item) ++ this

}


/**
 * Comanion for the lazy sequences.
 */
object LazySeq {

  /** Empty collection implementation. */
  private object Empty extends LazySeq[Nothing] {
    override def iterator() : Iterator[Nothing] = Iterator.empty
    override def ++[T1](other : LazySeq[T1]) : LazySeq[T1] = other
    override def filter(predicate : Nothing ⇒ Boolean) = this
  }

  /**
   * Instance of only one item.
   */
  private class Singleton[T](item : T) extends LazySeq[T] {
    override def iterator() : Iterator[T] = Iterator.single(item)
  }


  /** Multielement sequence. */
  private class Multiple[T](items : Seq[T]) extends LazySeq[T] {
    override def iterator() : Iterator[T] = Iterator(items : _*)
  }


  /** Single-item sum of elements. */
  private class SingleSum[T](left : LazySeq[T], right : LazySeq[T])
      extends LazySeq[T] {
    override def iterator() : Iterator[T] =
      left.iterator ++ right.iterator
  }



  private class FilteredSeq[T](base : LazySeq[T], predicate : T ⇒ Boolean)
      extends LazySeq[T] {
    override def iterator() : Iterator[T] =
      base.iterator.filter(predicate)
  }


  /** Mapping sequence. */
  private class MapSeq[T, R](base : LazySeq[T], mapper : T ⇒ R)
      extends LazySeq[R] {
    override def iterator() : Iterator[R] =
      base.iterator.map(mapper)
  }


  /** Sum of multiple elements. */
  private class MultiSum[T](items : Seq[LazySeq[T]])
      extends LazySeq[T] {
    override def iterator() : Iterator[T] =
      for (
        item ← items.iterator;
        elt ← item.iterator)
        yield elt
  }


  /** Empty sequence. */
  val empty : LazySeq[Nothing] = Empty

  /** Creates a new lazy sequence. */
  def make[T](items : T*) : LazySeq[T] =
    items match {
      case Seq() ⇒ empty
      case Seq(x) ⇒ new Singleton(x)
      case list ⇒ new Multiple(list)
    }

  /** Aggregates several sequences into one. */
  def sum[T](items : LazySeq[T]*) : LazySeq[T] =
    items match {
      case Seq() ⇒ empty
      case Seq(a) ⇒ a
      case Seq(a, b) ⇒ a ++ b
      case list ⇒ new MultiSum(list)
      /* Probably we can implement filtering of empty elements here. */
    }
}
