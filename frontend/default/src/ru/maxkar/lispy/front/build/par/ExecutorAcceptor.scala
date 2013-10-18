package ru.maxkar.lispy.front.build.par

import ru.maxkar.lispy.front.build._

import java.util.concurrent._

/** Acceptor, which sends tasks into the executor. */
private[par] final class ExecutorAcceptor(executor : Executor)
    extends TaskAcceptor {
  override def accept(task : ⇒  Unit) : Unit = {
    executor.execute(new Runnable() {
        override def run() : Unit = {
          task
        }
      })
  }
}
