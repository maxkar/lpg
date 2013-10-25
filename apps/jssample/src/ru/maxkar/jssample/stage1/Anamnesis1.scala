package ru.maxkar.jssample.stage1

import ru.maxkar.jssample.Anamnesis
import ru.maxkar.jssample._

import ru.maxkar.scoping._

/** Anamnesis of a first compilation stage.
 * @param duplicateDeclarations list of duplicate declarations.
 */
final class Anamnesis1(
      val duplicateDeclarations : Seq[DuplicateDeclarationInfo])
    extends Anamnesis {


  override def preventsCodegen() : Boolean =
    !duplicateDeclarations.isEmpty


  override def toString() : String =
    "    Dupes: " + duplicateDeclarations.size + "\n"
}
