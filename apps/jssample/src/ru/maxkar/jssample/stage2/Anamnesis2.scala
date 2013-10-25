package ru.maxkar.jssample.stage2

import ru.maxkar.jssample.{stage1 ⇒ S1}
import ru.maxkar.jssample._

final class Anamnesis2(
      val moddecls : Seq[DuplicateModuleDefinition])
    extends Anamnesis {

  def preventsCodegen() : Boolean = !moddecls.isEmpty
}
