package ru.maxkar.comp.vars

/**
 * Reference to _used_ variable.
 * @param D type of the variable "desriptor". Usually name, scope, etc...
 * @param C type of the variable refenence context. Usually it is a source
 *   location.
 * @param descriptor variable use descriptor.
 * @param context variable use context.
 */
class Reference[D, C] private (
    baseIdentity : AnyRef,
    val descriptor : D,
    val context : C) {

  /**
   * Variable identity. All variables derived from this instance using
   * any of "map" function will have essentialy the same identity. This
   * allows library user to "transform" references and context and still
   * keep an "origin" id.
   * <p>Meaning of the identity is undefined except it is a unique identifier
   * of all the variable transformed replicas.
   * <p> implementation of this method may change without a notice.
   */
  val identity : AnyRef =
    if (baseIdentity == null) this else baseIdentity

  /**
   * Maps descriptor using a given mapper function.
   * Result will have same identity as this variable.
   */
  def mapDescriptor[R](mapper : D ⇒ R) : Reference[R, C] =
    new Reference(identity, mapper(descriptor), context)
}


/**
 * Companion object for the reference class.
 */
object Reference {

  /**
   * Creates a new variable reference.
   * This reference will have a new and unique identity.
   */
  def apply[D, C](descriptor : D, context : C) : Reference[D, C] =
    new Reference(null, descriptor, context)
}
