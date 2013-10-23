package ru.maxkar.hunk

/** Network iterable item.
 * @param peers peer (child) items.
 * @param ownItems items for this net iterable.
 */
private[hunk] final class NetIterable[+T](
    private[hunk] val peers : Seq[NetIterable[T]],
    private[hunk] val ownItems : Seq[T]) {

  private[hunk] val res : Iterable[T] =  {
    val p = new NetIterableBuilder[T]
    p.populate(this)
    p.items.flatten
  }

}
