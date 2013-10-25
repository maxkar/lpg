package ru.maxkar.jssample

/** Anamnesis for the processing stage.
 * Provides severity information for that
 * stage.
 */
trait Anamnesis {
  /** Checks, if this anamnesis prevents code generation
   * phase. For example, duplicate local declarations may
   * prevent a code generation phase, but not lookup phase.
   */
  def preventsCodegen() : Boolean
}
