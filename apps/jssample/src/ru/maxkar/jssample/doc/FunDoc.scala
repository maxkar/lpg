package ru.maxkar.jssample.doc

/** Function documentation. */
final case class FunDoc(
    name : String,
    globalName : Option[String],
    isPublic : Boolean,
    contract : DocBody,
    args : Seq[ArgDoc],
    ret : DocBody)
