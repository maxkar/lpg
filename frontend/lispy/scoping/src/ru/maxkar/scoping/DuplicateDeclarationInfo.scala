package ru.maxkar.scoping

import ru.maxkar.lispy.Attributes

/** Information about duplicate declaration.
 * @param name name of the item.
 * @param first first occurence information.
 * @param next next occurence information.
 */
final case class DuplicateDeclarationInfo(
  name : String,
  first : Attributes,
  next : Attributes)
