package ru.maxkar.jssample.stage2

import ru.maxkar.scoping._
import ru.maxkar.jssample.ns._

final class Result(
  val mods : Map[Seq[String], Namespace[Reference]],
  val anamnesis : Anamnesis2)
