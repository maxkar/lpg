package ru.maxkar.hunk


/**
 * Piece of work with possible success, failure
 * and execution traces. Note, that hunk may not have
 * a readily-available result at any given time. Such
 * result may be computed asynchronously/in parallel.
 */
abstract class Hunk[+R] {
  import Hunk._

  /** Calculated a "dirty" immediate result.
   * Returns null if there is no result yet. Usefull
   * for the hunk implementations.
   */
  protected def dirtyResult() : HunkResult[R]


  /** Tries to get a result if it already avialable. */
  final def immediateResult() : Option[HunkResult[R]] = {
    val r = dirtyResult
    if (r == null)
      None
    else
      Some(r)
  }


  /** Blocking result accessor. */
  def awaitResult() : HunkResult[R] = {
    val waiter = new HunkResultWaiter[HunkResult[R]]
    onComplete(waiter.publish)
    waiter.await
  }


  /** Invokes a callback on the hunk result when
   * hunk receives its result. This methods may
   * be invoked immediately if hunk already have some
   * result. Callback may be called before returning
   * from the call to onComplete.
   */
  def onComplete(handler : HunkResult[R] ⇒  Unit) : Unit


  /** Invokes a handler on the thunk result.
   * Handler is invoked only when hunk completes successfully. */
  def onResult(handler : R ⇒ Unit) : Unit = {
    onComplete(res ⇒ {
        res match {
          case HunkSuccess(r) ⇒ handler(r)
          case _ ⇒ ()
        }
      })
  }


  /** Applies a function to a successfull result of
   * this thunk.
   */
  def fmap[R1](f : R ⇒ R1) : Hunk[R1] = {
    val res = new DeferredHunk[R1]
    onComplete(r ⇒
      r match {
        case a@HunkSuccess(r) ⇒
          try {
            res.succeed(f(r))
          } catch {
            case t : Throwable ⇒ res.fail(t)
          }
        case e@HunkException(exn) ⇒ res.fail(exn)
      })
    res
  }


  /** Simple application. */
  def <*[R2](other : Hunk[R2]) : Hunk[R] = fmap(Function.const _) <*> other
  def *>[R2](other : Hunk[R2]) : Hunk[R2] = fmap(Hunk.snd[R, R2]) <*> other
}


/** Simple hunk utilities. */
object Hunk {
  import scala.language.implicitConversions
  import scala.collection.mutable.ArrayBuffer
  import java.util.concurrent.Executor


  /** Create a hunk with a fixed success value. */
  def succHunk[R](res : R) : Hunk[R] =
    new ImmediateHunk[R](new HunkSuccess[R](res))


  /** Creats a hunk which already fails. */
  def failHunk[R](fail : Throwable) : Hunk[R] =
    new ImmediateHunk[R](new HunkException[R](fail))


  /** Calculates a hunk in synchronous way. */
  def calc[R](block : ⇒ R) : Hunk[R] =
    try {
      succHunk(block)
    } catch {
      case x : Throwable ⇒ failHunk(x)
    }


  /** Creates a hunk which executes in asynchronous way
   * using implicit executor.
   */
  def exec[R](block : ⇒ R)(implicit executor : Executor) : Hunk[R] = {
    val res = new DeferredHunk[R]
    executor.execute(new Runnable() {
        override def run() : Unit = {
          try {
            res.succeed(block)
          } catch {
            case x : Throwable ⇒ res.fail(x)
          }
        }
      })
    res
  }


  /** Awaits all hunks and returns it's results. */
  def awaitAll[R](hunks : Seq[Hunk[R]]) : Seq[HunkResult[R]] = {
    hunks.map(x ⇒  x.awaitResult)
  }


  /** Awaits all hunks and splits results into "successes" and "failures".
   */
  def awaitSplit[R](hunks : Seq[Hunk[R]]) :
      (Seq[Throwable], Seq[R]) = {

    val succs = new ArrayBuffer[R](hunks.length)
    val fails = new ArrayBuffer[Throwable]

    awaitAll(hunks).foreach(x ⇒ x match {
        case HunkException(exn) ⇒ fails += exn
        case HunkSuccess(v) ⇒ succs += v
      })
    (fails, succs)
  }


  /** Applicative operation. */
  implicit class ApplicativeHunk[R1, R2](
        val x : Hunk[R1 ⇒ R2]) extends AnyVal {
    def <*>(other : Hunk[R1]) : Hunk[R2] = {
      val res = new DeferredHunk[R2]

      x.onComplete(x1 ⇒
        x1 match {
          case HunkException(exn) ⇒ res.fail(exn)
          case HunkSuccess(r1) ⇒
            other.onComplete(x2 ⇒
              x2 match {
                case HunkException(exn) ⇒ res.fail(exn)
                case HunkSuccess(r2) ⇒
                  try {
                    res.succeed(r1(r2))
                  } catch {
                    case t : Throwable ⇒ res.fail(t)
                  }
              })
        })
      res
    }
  }



  /** Applicative function operation. */
  implicit class ApplicativeFunction[F1, F2](
        val x : F1 ⇒  F2) extends AnyVal {
    @inline
    def <*>(other : Hunk[F1]) : Hunk[F2] =
      other.fmap(x)
  }



  /** Monadic function operation. */
  implicit class MonadicFunction[R1, R2](
      val x : R1 ⇒ Hunk[R2]) extends AnyVal {

    def <**>(other : Hunk[R1]) : Hunk[R2] = {
      val res = new DeferredHunk[R2]

      other.onComplete(base ⇒ base match {
          case HunkException(exn) ⇒ res.fail(exn)
          case HunkSuccess(r) ⇒
            try {
              x(r).onComplete(res.resolve)
            } catch {
              case x : Throwable ⇒ res.fail(x)
            }
        })

      res
    }
  }


  /** Super monadic composition. */
  implicit class SuperMonadicFunction[R1, R2](
       val x : Hunk[R1 ⇒ Hunk[R2]]) extends AnyVal {

    def <**>(other : Hunk[R1]) : Hunk[R2] = {
      val res = new DeferredHunk[R2]

      x.onComplete(r1 ⇒
        r1 match {
          case HunkException(exn) ⇒ res.fail(exn)
          case HunkSuccess(v1) ⇒
            other.onComplete(r2 ⇒
              r2 match {
                case HunkException(exn) ⇒ res.fail(exn)
                case HunkSuccess(v2) ⇒
                  try {
                    v1(v2).onComplete(res.resolve)
                  } catch {
                    case exn : Throwable ⇒ res.fail(exn)
                  }
              }
            )
        })
      res
    }
  }


  /** Returns a second argument. Utility function. */
  private def snd[V1, V2](v1 : V1)(v2 : V2) : V2 = v2
}
