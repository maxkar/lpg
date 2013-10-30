package ru.maxkar.jssample.ns

import ru.maxkar.lispy.parser.TextPosition
import java.io.File

/** Place of item declaration. */
abstract sealed class DeclarationHost


/** Item is declared at a system level. */
final case object SystemHost extends DeclarationHost


/** Object was defined in a file. */
final case class ModuleHost(file : File, offset : TextPosition)
  extends DeclarationHost
