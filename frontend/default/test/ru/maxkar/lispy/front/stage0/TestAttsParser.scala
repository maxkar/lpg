package ru.maxkar.lispy.front.stage0.parser

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.JUnitSuite

import ru.maxkar.lispy.front._
import ru.maxkar.lispy.front.Attributes._
import ru.maxkar.lispy.front.stage0._
import ru.maxkar.lispy.front.parser._

import org.junit._
import org.junit.Assert._

final class TestAttsParser extends JUnitSuite {
  import scala.language.implicitConversions

  private val a1 = new Attribute[Int]("a1")
  private val s2 = new Attribute[String]("s2")

  private def pa1(loc : Attributes, input : Input) : Attributes = {
    input.dropWhites
    val digs = input.charsWhile(Character.isDigit)
    singleton(a1, Integer.parseInt(digs))
  }

  private def ps2(loc : Attributes, input : Input) : Attributes = {
    input.dropWhites
    val digs = input.charsWhile(Character.isJavaIdentifierPart)
    singleton(s2, digs)
  }

  @Test
  def testAttparse() : Unit = {
    assertEquals(empty + (a1, 123), p("{a1: 123  }  "))
    assertEquals(empty + (a1, 123) + (a1, 321), p("{a1: 123  } {a1 : 321} "))
    assertEquals(empty + (s2, "test") + (a1, 321), p("{s2: test  } {a1 : 321} "))
  }


  val ppf = AttsParser.fromFactory(Map(
    "a1" → pa1 _,
    "s2" → ps2 _
  ).get)

  private def p(input : String) : Attributes = {
    SParser.parseSFile(ppf)(Input.fromCharArray(input.toCharArray())).atts
  }
}
