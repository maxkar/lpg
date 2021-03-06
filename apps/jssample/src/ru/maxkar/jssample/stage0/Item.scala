package ru.maxkar.jssample.stage0

import java.io.File

import ru.maxkar.lispy._


/** Stage-0 output item. */
final case class Item(
    source : File,
    path : Seq[String],
    body : SList[BaseItem])

