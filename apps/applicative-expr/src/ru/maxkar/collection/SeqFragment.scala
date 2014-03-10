package ru.maxkar.collection

/**
 * Sequence or sequence fragment.
 * This class represents some item which can be treated
 * as an immutable sequence. Unlike scala classes this class
 * does not support generic "can build" operations and it not
 * traversible-like. Instead this class offer efficient
 * concatenation of O(i) complexity where i is a number of
 * elements in the concatenation.
 *
 * <p>This class offers also a "fast" filtering, which does
 * not traverse items on the filtering time. Instead items
 * will be filtered during the iteration. And this class does
 * not cache filtering results. So if filter rejects many
 * elements and there are several iterations, user should store
 * result himself.
 *
 * <p>Standard usecase for this class is "flattening" tree to
 * a simple list in an immutable manner. You do not need
 * tree collectors and may store "aggregate" results in each
 * intermediate node.
 *
 * @param T element type.
 */
abstract class SeqFragment[+T] private[collection]() {
  /** Iterator over elements of this collection. */
  def iterator() : Iterator[T]

  /** Creates a filtered fragment. The fragment returns only items
   * matching a predicate.
   * <p> See class-level documentation to find memory/time restriction.
   * @param predicate filtering predicate. This predicate should return
   *   true for items which should remain in the sequence.
   */
  def filter(predicate : T ⇒ Boolean) : SeqFragment[T] =
    new FilteringSeqFragment(this, predicate)
}


/** Fragment companion object. */
object SeqFragment {

  /** Empty fragment. */
  val empty : SeqFragment[Nothing] = new SeqFragment[Nothing] {
    override def iterator() : Iterator[Nothing] = Iterator.empty
    override def filter(predicate : Nothing ⇒ Boolean)
      : SeqFragment[Nothing]
      = this
  }


  /** Creates fragment from a single item. */
  def from[T](item : T) : SeqFragment[T] =
    new SeqFragment[T] {
      override def iterator() : Iterator[T] =
        Iterator single item
    }


  /** Creates fragment from an iterable items. */
  def fromIterable[T](items : Iterable[T]) : SeqFragment[T] =
    new SeqFragment[T] {
      override def iterator() : Iterator[T] =
        items.iterator
    }


  /** Creates fragment from a sequence. */
  def fromSeq[T](items : Seq[T]) : SeqFragment[T] =
    items match {
      case Seq() ⇒ empty
      case Seq(x) ⇒ from(x)
      case _ ⇒ fromIterable(items)
    }


  /** Vararg version of a fragment. */
  def fromV[T](items : T*) : SeqFragment[T] =
    fromSeq(items)


  /** Combines several fragments into a one big fragment. */
  def seq[T](items : Seq[SeqFragment[T]]) : SeqFragment[T] =
    items match {
      case Seq() ⇒ empty
      case Seq(x) ⇒ x
      case _ ⇒
        new ComposingSeqFragment(items)
    }


  /** Vararg version of seq. */
  def seqV[T](items : SeqFragment[T]*) : SeqFragment[T] =
    seq(items)


  /** Filters a sequence using a predicate. */
  def filter[T](
        items : SeqFragment[T],
        predicate : T ⇒ Boolean)
      : SeqFragment[T] =
    items filter predicate
}



/* Utilities. */


/** Composing sequence implementation. */
private final class ComposingSeqFragment[T](items : Seq[SeqFragment[T]])
    extends SeqFragment[T] {
  override def iterator() : Iterator[T] =
    for (
      item ← items.iterator;
      elt ← item.iterator)
    yield elt
}


/** Filtering sequence implementation. */
private final class FilteringSeqFragment[T](
      base : SeqFragment[T],
      predicate : T ⇒ Boolean)
    extends SeqFragment[T] {

  override def iterator() : Iterator[T] =
    for (item ← base.iterator if predicate(item))
      yield item
}
