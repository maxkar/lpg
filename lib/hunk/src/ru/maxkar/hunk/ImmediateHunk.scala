package ru.maxkar.hunk



/** Immediate hunk result. This thunk have some value. */
private[hunk] final class ImmediateHunk[R, T](value : HunkResult[R, T])
    extends Hunk[R, T] {

  override protected def dirtyResult() : HunkResult[R, T] = value

  override def onComplete(handler : HunkResult[R, T] ⇒  Unit) : Unit =
    handler(value)

  override def awaitResult() : HunkResult[R, T] = value
}
