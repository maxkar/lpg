package ru.maxkar.jssample.stage1

import ru.maxkar.alias.collection._

import ru.maxkar.lispy._

import java.io._

/** Stage processing result.
 * @param source source file.
 * @param module local module name.
 * @param entity processed entity.
 * @param anamnesis anamnesis of the entity.
 */
final class Result(
      val source : File,
      val module : Seq[String],
      val entity : SExpression[BaseItem],
      val decls : JSet[String],
      val anamnesis : Anamnesis1){

  override def toString() : String =
    "Result1 of " + source + ":\n  Anamnesis:\n" + anamnesis
}
