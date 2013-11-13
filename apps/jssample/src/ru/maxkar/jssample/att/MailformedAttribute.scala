package ru.maxkar.jssample.att

import ru.maxkar.lispy._

/** Mailformed attribute specificaiton. */
abstract class MailformedAttribute(msg : String) extends Exception(msg) {
  val location : Attributes
}
