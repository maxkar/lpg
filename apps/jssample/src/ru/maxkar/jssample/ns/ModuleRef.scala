package ru.maxkar.jssample.ns

import ru.maxkar.lispy.parser.TextPosition

/** Reference to a module. */
final case class ModuleRef(name : String, loc : TextPosition)
