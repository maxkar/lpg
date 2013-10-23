package ru.maxkar.hunk

/** Waiter for the hunk result. */
private[hunk] class HunkResultWaiter[T >: Null] {
  private var haveResult = false
  private var result : T = null


  /** Awaits the result. */
  def await() : T = {
    this synchronized {
      while (!haveResult)
        wait
    }
    result
  }


  /** Publishes a hunk result. */
  def publish(res : T) : Unit = {
    this synchronized {
      if (haveResult)
        throw new IllegalStateException("Result is already published")
      result = res
      haveResult = true
      notifyAll
    }
  }
}
