package ru.maxkar.jssample.doc

/**
 * Variable documentation.
 * @param name variable name.
 * @param globalName optional global name for the global variables.
 * @param doc variable documentation.
 */
final case class VarDoc(
    name : String,
    globalName : Option[String],
    isPublic : Boolean,
    doc : DocBody)
