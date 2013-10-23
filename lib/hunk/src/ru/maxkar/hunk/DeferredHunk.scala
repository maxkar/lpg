package ru.maxkar.hunk

import scala.collection.mutable.ArrayBuffer


/** Deferred hunk. Base for async (and many other) task implementations.
 * User must manually mark this hunk as "resolved" with some
 * particular value.
 */
final class DeferredHunk[R, T] extends Hunk[R, T] {
  /** List of termination callbacks. */
  private var waiters = new ArrayBuffer[HunkResult[R, T] ⇒ Unit]
  /** Dirty result value. */
  @volatile private var result : HunkResult[R, T] = null

  override protected def dirtyResult() : HunkResult[R, T] = result

  override def onComplete(cb : HunkResult[R, T] ⇒  Unit) : Unit = {
    this synchronized {
      var wlist = waiters
      /* Not-complete check. */
      if (wlist != null) {
        wlist += cb
        return
      }
    }

    cb(result)
  }


  /** Pushes a result and marks this hunk as resolved with
   * this particular result.
   */
  def resolve(res : HunkResult[R, T]) : Unit = {
    val cblist =
      this synchronized {
        if (waiters == null)
          throw new IllegalStateException("Result is already set")
        val cbres = waiters
        /* Clean memory. */
        waiters = null
        result = res
        cbres
      }

    /* Process deferred callbacks. */
    cblist.foreach(
      x ⇒ try {
        x(res)
      } catch {
        case exn : Throwable ⇒ exn.printStackTrace()
      })
  }


  /** Succeedes a hunk with a list of dependencies
   * and traces. */
  def succeedDT(
        deps : Seq[HunkResult[_, T]],
        traces : Seq[T],
        res : R)
      : Unit =
    resolve(HunkSuccess(deps, traces, res))


  /** Succeedes a hunk with a trace. */
  def succeedT(traces : Seq[T], res : R) : Unit =
    resolve(HunkSuccess(Seq.empty, traces, res))


  /** Succeedes a hunk with dependencies. */
  def succeedD(deps : Seq[HunkResult[_, T]], res : R) : Unit =
    resolve(HunkSuccess(deps, Seq.empty, res))


  /** Succeedes a hunk with no traces or dependencies. */
  def succeed(res : R) : Unit = resolve(HunkSuccess(Seq.empty, Seq.empty, res))


  /** Fails a hunk with a given dependencies and traces. */
  def failDT(
        deps : Seq[HunkResult[_, T]],
        traces : Seq[T],
        res : Throwable)
      : Unit =
    resolve(HunkException(deps, traces, res))


  /** Fails a hunk with a trace. */
  def failT(traces : Seq[T], res : Throwable) : Unit =
    resolve(HunkException(Seq.empty, traces, res))


  /** Fails a hunk with dependencies. */
  def failD(deps : Seq[HunkResult[_, T]], res : Throwable) : Unit =
    resolve(HunkException(deps, Seq.empty, res))


  /** Fails a hunk with no traces or dependencies. */
  def fail(res : Throwable) : Unit = resolve(HunkException(Seq.empty, Seq.empty, res))

}
