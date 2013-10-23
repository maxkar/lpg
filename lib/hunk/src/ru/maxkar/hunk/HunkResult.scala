package ru.maxkar.hunk


/** Hunk execution result.
 * @param R result type.
 * @param T trace type.
 */
abstract sealed class HunkResult[+R, +T] protected(
      peers : Seq[HunkResult[_, T]],
      ownExn : Option[Throwable],
      ownTraces : Seq[T]) {


  /** Exception network iterator. */
  private val exceptionsNet : NetIterable[Throwable] =
    new NetIterable[Throwable](
      peers.map(x ⇒ x.exceptionsNet),
      ownExn.toSeq)


  /** Tracing network. */
  private val traceNet : NetIterable[T] =
    new NetIterable[T](
      peers.map(x ⇒ x.traceNet),
      ownTraces)


  /** All exceptions which occured during evalutation of this hunk. */
  val allExceptions : Iterable[Throwable] = exceptionsNet.res
}


/** Hunk execution success. */
final case class HunkSuccess[+R, +T](
    peers : Seq[HunkResult[_, T]],
    traces : Seq[T],
    value : R)
  extends HunkResult[R, T](peers, None, traces)


/** Hunk failed with an exception. */
final case class HunkException[+R, +T](
      peers : Seq[HunkResult[_, T]],
      traces : Seq[T],
      exception : Throwable)
    extends HunkResult[R, T](peers, Some(exception), traces)
