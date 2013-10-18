package ru.maxkar.lispy.front.build.par

import  ru.maxkar.lispy.front.build._

import java.util.concurrent._

/** Accessors to executors. */
object TaskAcceptors {
  /** Creates a new task acceptor from executor. */
  def fromExecutor(executor : Executor) : TaskAcceptor =
    new ExecutorAcceptor(executor)


  /** Creates a new virtual task group with a
   * provided acceptor as a base.
   */
  def createGroup(base : TaskAcceptor) : ExecutionGroup =
    new ExecutionGroup(base)
}
