package ru.maxkar.lispy.front


/**
 * Attribute id (key). Each attribute is identified by
 * it's "identity" value, not a display name or something
 * like that.
 */
final class Attribute[T: Manifest](val displayName : String) {
  override def toString() : String = "Attribute [" + displayName + "]"
}
