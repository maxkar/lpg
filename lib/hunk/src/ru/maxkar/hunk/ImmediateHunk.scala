package ru.maxkar.hunk



/** Immediate hunk result. This thunk have some value. */
private[hunk] final class ImmediateHunk[R](value : HunkResult[R])
    extends Hunk[R] {

  override protected def dirtyResult() : HunkResult[R] = value

  override def onComplete(handler : HunkResult[R] ⇒  Unit) : Unit =
    handler(value)

  override def awaitResult() : HunkResult[R] = value
}
