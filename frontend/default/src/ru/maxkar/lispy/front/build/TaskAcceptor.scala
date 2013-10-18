package ru.maxkar.lispy.front.build

/** Base trait for all build items
 * which accepts tasks for processing.
 */
trait TaskAcceptor {
  /** Accepts a task for the execution.
   * This trait does not specify how a task
   * should be processed. Task may be processed
   * in executor thread, it may be processed in
   * current thread, it may be ignored at all.
   */
  def accept(task : ⇒ Unit) : Unit
}
