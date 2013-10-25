package ru.maxkar.hunk

import scala.collection.mutable.ArrayBuffer


/** Deferred hunk. Base for async (and many other) task implementations.
 * User must manually mark this hunk as "resolved" with some
 * particular value.
 */
final class DeferredHunk[R] extends Hunk[R] {
  /** List of termination callbacks. */
  private var waiters = new ArrayBuffer[HunkResult[R] ⇒ Unit]
  /** Dirty result value. */
  @volatile private var result : HunkResult[R] = null

  override protected def dirtyResult() : HunkResult[R] = result

  override def onComplete(cb : HunkResult[R] ⇒  Unit) : Unit = {
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
  def resolve(res : HunkResult[R]) : Unit = {
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


  /** Succeedes a hunk with no traces or dependencies. */
  def succeed(res : R) : Unit = resolve(HunkSuccess(res))


  /** Fails a hunk with no traces or dependencies. */
  def fail(res : Throwable) : Unit = resolve(HunkException(res))

}
