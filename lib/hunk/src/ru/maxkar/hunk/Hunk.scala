package ru.maxkar.hunk


/**
 * Piece of work with possible success, failure
 * and execution traces. Note, that hunk may not have
 * a readily-available result at any given time. Such
 * result may be computed asynchronously/in parallel.
 */
abstract class Hunk[+R, +T] {
  import Hunk._

  /** Calculated a "dirty" immediate result.
   * Returns null if there is no result yet. Usefull
   * for the hunk implementations.
   */
  protected def dirtyResult() : HunkResult[R, T]


  /** Tries to get a result if it already avialable. */
  final def immediateResult() : Option[HunkResult[R, T]] = {
    val r = dirtyResult
    if (r == null)
      None
    else
      Some(r)
  }


  /** Blocking result accessor. */
  def awaitResult() : HunkResult[R, T] = {
    val waiter = new HunkResultWaiter[HunkResult[R, T]]
    onComplete(waiter.publish)
    waiter.await
  }


  /** Invokes a callback on the hunk result when
   * hunk receives its result. This methods may
   * be invoked immediately if hunk already have some
   * result. Callback may be called before returning
   * from the call to onComplete.
   */
  def onComplete(handler : HunkResult[R, T] ⇒  Unit) : Unit


  /** Invokes a handler on the thunk result.
   * Handler is invoked only when hunk completes successfully. */
  def onResult(handler : R ⇒ Unit) : Unit = {
    onComplete(res ⇒ {
        res match {
          case HunkSuccess(_, _, r) ⇒ handler(r)
          case _ ⇒ ()
        }
      })
  }


  /** Applies a function to a successfull result of
   * this thunk.
   */
  def fmap[R1](f : R ⇒ R1) : Hunk[R1, T] = {
    val res = new DeferredHunk[R1, T]
    onComplete(r ⇒
      r match {
        case a@HunkSuccess(_, _, r) ⇒
          try {
            res.succeedD(Seq(a), f(r))
          } catch {
            case t : Throwable ⇒
              res.failD(Seq(a), t)
          }
        case e@HunkException(_, _, exn) ⇒
          res.failD(Seq(e), exn)
      })
    res
  }


  /** Simple application. */
  def <*[R2, T1 >: T](other : Hunk[R2, T1]) : Hunk[R, T1] = fmap(Function.const _) <*> other
  def *>[R2, T1 >: T](other : Hunk[R2, T1]) : Hunk[R2, T1] = fmap(Hunk.snd[R, R2]) <*> other
}


/** Simple hunk utilities. */
object Hunk {
  import scala.language.implicitConversions
  import scala.collection.mutable.ArrayBuffer
  import java.util.concurrent.Executor


  /** Create a hunk with a fixed success value. */
  def succHunkT[R, T](res : R, trace : Seq[T]) : Hunk[R, T] =
    new ImmediateHunk[R, T](new HunkSuccess[R, T](Seq.empty, trace, res))


  /** Creats a hunk which already fails. */
  def failHunkT[R, T](fail : Throwable, trace : Seq[T]) : Hunk[R, T] =
    new ImmediateHunk[R, T](new HunkException[R, T](Seq.empty, trace, fail))


  /** Calculates a hunk in synchronous way. */
  def calc[R, T](block : ⇒ R) : Hunk[R, T] =
    try {
      succHunkT(block, Seq.empty)
    } catch {
      case x : Throwable ⇒
        failHunkT(x, Seq.empty)
    }


  /** Calculates a hunk in synchronous way with tracing.
   * Tracer in the body is not thread-safe.
   */
  def calcT[R, T](body : (T ⇒ Unit) ⇒ R) : Hunk[R, T] = {
    val tracer = new ArrayBuffer[T]
    try {
      val res = body(tracer.+=)
      succHunkT(res, tracer.toSeq)
    } catch {
      case x : Throwable ⇒
        failHunkT(x, tracer.toSeq)
    }
  }


  /** Creates a hunk which executes in asynchronous way
   * using implicit executor.
   */
  def exec[R, T](block : ⇒ R)(implicit executor : Executor) : Hunk[R, T] = {
    val res = new DeferredHunk[R, T]
    executor.execute(new Runnable() {
        override def run() : Unit = {
          try {
            res.succeed(block)
          } catch {
            case x : Throwable ⇒
              res.fail(x)
          }
        }
      })
    res
  }


  /** Creates a traceable hunk which executes in asynchronous way
   * using implicit executor.
   */
  def execT[R, T](body : (T ⇒ Unit) ⇒ R)(implicit executor : Executor) : Hunk[R, T] = {
    val res = new DeferredHunk[R, T]
    executor.execute(new Runnable() {
        override def run() : Unit = {
          val tracer = new ArrayBuffer[T]
          try {
            val v = body(tracer.+=)
            res.succeedT(tracer.toSeq, v)
          } catch {
            case x : Throwable ⇒
              res.failT(tracer.toSeq, x)
          }
        }
      })
    res
  }


  /** Awaits all hunks and returns it's results. */
  def awaitAll[R, T](hunks : Seq[Hunk[R, T]]) : Seq[HunkResult[R, T]] = {
    hunks.map(x ⇒  x.awaitResult)
  }


  /** Awaits all hunks and splits results into "successes" and "failures".
   */
  def awaitSplit[R, T](hunks : Seq[Hunk[R, T]]) :
      (Seq[(Throwable, Iterable[T])], Seq[(R, Iterable[T])]) = {

    val succs = new ArrayBuffer[(R, Iterable[T])](hunks.length)
    val fails = new ArrayBuffer[(Throwable, Iterable[T])]

    awaitAll(hunks).foreach(x ⇒ x match {
        case HunkException(_, _, exn) ⇒
          fails += ((exn, x.allTraces))
        case HunkSuccess(_, _, v) ⇒
          succs += ((v, x.allTraces))
      })
    (fails, succs)
  }


  /** Applicative operation. */
  implicit class ApplicativeHunk[R1, R2, T](
        val x : Hunk[R1 ⇒ R2, T]) extends AnyVal {
    def <*>[T2 >: T](other : Hunk[R1, T2]) : Hunk[R2, T2] = {
      val res = new DeferredHunk[R2, T2]

      x.onComplete(x1 ⇒
        other.onComplete(x2 ⇒ {
          x1 match {
            case HunkException(_, _, exn) ⇒
              res.failD(Seq(x1, x2), exn)
            case HunkSuccess(_, _, r1) ⇒
              x2 match {
                case HunkException(_, _, exn) ⇒
                  res.failD(Seq(x1, x2), exn)
                case HunkSuccess(_, _, r2) ⇒
                  try {
                    res.succeedD(Seq(x1, x2), r1(r2))
                  } catch {
                    case t : Throwable ⇒
                      res.failD(Seq(x1, x2), t)
                  }
              }
          }
        }))

      res
    }
  }



  /** Applicative function operation. */
  implicit class ApplicativeFunction[F1, F2](
        val x : F1 ⇒  F2) extends AnyVal {
    @inline
    def <*>[T](other : Hunk[F1, T]) : Hunk[F2, T] =
      other.fmap(x)
  }



  /** Monadic function operation. */
  implicit class MonadicFunction[R1, R2, T](
      val x : R1 ⇒ Hunk[R2, T]) extends AnyVal {

    def <**>(other : Hunk[R1, T]) : Hunk[R2, T] = {
      val res = new DeferredHunk[R2, T]

      other.onComplete(base ⇒ base match {
          case a@HunkSuccess(_, _, r) ⇒
            try {
              val sub = x(r)

              sub.onComplete(subr ⇒  subr match {
                  case a1@HunkSuccess(_, _, r) ⇒
                    res.succeedD(Seq(a, a1), r)
                  case f@HunkException(_, _, e) ⇒
                    res.failD(Seq(a, f), e)
              })
            } catch {
              case x : Throwable ⇒
                res.failD(Seq(a), x)
            }
          case f@HunkException(_, _, exn) ⇒
            res.failD(Seq(f), exn)
        })

      res
    }
  }


  /** Super monadic composition. */
  implicit class SuperMonadicFunction[R1, R2, T](
       val x : Hunk[R1 ⇒ Hunk[R2, T], T]) extends AnyVal {

    def <**>(other : Hunk[R1, T]) : Hunk[R2, T] = {
      val res = new DeferredHunk[R2, T]

      x.onComplete(r1 ⇒
          other.onComplete(r2 ⇒ {
              r1 match {
                case a1@HunkException(_, _, exn) ⇒
                  res.failD(Seq(a1, r2), exn)
                case a1@HunkSuccess(_, _, v1) ⇒
                  r2 match {
                    case a2@HunkException(_, _, exn) ⇒
                      res.failD(Seq(a1, a2), exn)
                    case a2@HunkSuccess(_, _, v2) ⇒
                      try {
                        val sub = v1(v2)
                        sub.onComplete(subr ⇒  subr match {
                            case a3@HunkSuccess(_, _, v3) ⇒
                              res.succeedD(Seq(a1, a2, a3), v3)
                            case a3@HunkException(_, _, exn) ⇒
                              res.failD(Seq(a1, a2, a3), exn)
                          })
                      } catch {
                        case exn : Throwable ⇒
                          res.failD(Seq(r1, r2), exn)
                      }
                  }
              }
            }
        ))
      res
    }
  }


  /** Returns a second argument. Utility function. */
  private def snd[V1, V2](v1 : V1)(v2 : V2) : V2 = v2
}
