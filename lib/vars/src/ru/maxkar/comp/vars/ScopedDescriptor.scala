package ru.maxkar.comp.vars

/**
 * Descriptor of a variable inside a given scope. Scope can
 * further be used during a lookup process.
 * @param S scope descriptor.
 * @param name source variable name.
 * @param scope defined variable scope.
 */
class ScopedDescriptor[S] private(
    val name : String,
    val scope : S) {

  /**
   * Maps a scope using a given function.
   */
  def mapScope[R](mapper : S ⇒ R) : ScopedDescriptor[R] =
    new ScopedDescriptor(name, mapper(scope))
}


/**
 * Companion for the scoped descriptor.
 */
object ScopedDescriptor {
  /**
   * Creates a new descriptor.
   */
  def apply[S](name : String, scope : S) : ScopedDescriptor[S] =
    new ScopedDescriptor(name, scope)
}
