package ru.maxkar.comp.vars

/**
 * Variable declaration.
 * @param D variable declaration desription.
 * @param C variable declaration context. Usually it a
 *  source location.
 * @param descriptor variable declaration descriptor.
 * @param context variable declaration context.
 */
class Declaration[D, C] private(
    val descriptor : D,
    val context : C) {
}

/**
 * Declaration companion.
 */
object Declaration {
  /**
   * Creates a new variable declaration.
   */
  def apply[D, C](descriptor : D, context : C) : Declaration[D, C] =
    new Declaration(descriptor, context)
}
