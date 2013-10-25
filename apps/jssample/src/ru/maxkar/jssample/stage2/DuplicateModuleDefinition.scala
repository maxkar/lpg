package ru.maxkar.jssample.stage2

import java.io.File

/** Module redefinition. */
final case class DuplicateModuleDefinition(
  name : Seq[String], first: File, second : File)
