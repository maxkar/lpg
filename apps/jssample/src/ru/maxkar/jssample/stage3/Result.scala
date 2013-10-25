package ru.maxkar.jssample.stage3

import ru.maxkar.lispy._
import ru.maxkar.jssample.ns._

import java.io.File

/** Stage processing result. */
final class Result(
  val source : File,
  val anamnesis : Anamnesis3,
  val item : SExpression[LookupNode]) {
}
