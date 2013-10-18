package ru.maxkar.lispy.front.build.par

import ru.maxkar.lispy.front.build._

/**
 * Acceptor which tracks list of running (and not completed)
 * tasks and provides a way to await condition when there
 * are no pending tasks.
 */
final class ExecutionGroup private[par](peer : TaskAcceptor)
    extends TaskAcceptor {

  /** Number of active tasks. */
  private var numActive = 0


  /** Access monitor. */
  private var lock = new Object


  /** Awaits until this execution group
   * is free of all waiting/running tasks.
   * Returning from this function does not mean that
   * no more tasks may be accepter. It's quite contary.
   * This group may accept new tasks and then be awaited
   * of that tasks completion.
   */
  def awaitFree() : Unit = {
    lock synchronized {
      while (numActive > 0)
        lock.wait
    }
  }



  override def accept(task : ⇒  Unit) {
    val runner = new TaskRunner(task)
    lock synchronized {
      numActive += 1
    }

    try {
      peer.accept(runner.exec)
    } catch {
      case e : Throwable ⇒
      runner.finish
      throw e
    }
  }


  /** Task wrapper. */
  private final class TaskRunner(item : ⇒ Unit) {


    /** "Task completed" flag. */
    private var complete = false


    /** Executes a task. */
    def exec() : Unit = {
      try {
        item
      } finally {
        finish
      }
    }


    /** Marks this task as finished. */
    def finish() : Unit = {
      this synchronized {
        if (complete)
          return
        complete = true
      }
      lock synchronized {
        numActive -= 1
        if (numActive == 0)
          lock.notifyAll
      }
    }
  }




}
