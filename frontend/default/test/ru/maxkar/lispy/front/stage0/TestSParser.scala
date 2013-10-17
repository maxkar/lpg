package ru.maxkar.lispy.front.stage0.parser

import ru.maxkar.lispy.front._
import ru.maxkar.lispy.front.Attributes._
import ru.maxkar.lispy.front.stage0._
import ru.maxkar.lispy.front.parser._

import org.junit._
import org.junit.Assert._

import java.math._

final class TestSParser {
  import scala.language.implicitConversions


  @Test
  def testSimpleExpr() : Unit = {
    e() =? "  (   )"
    e(e(), e()) =? "  ( () ())"
  }


  @Test
  def testIdExpr() : Unit = {
    i("abc") =? "abc"
    e(i("abc"), i("def")) =? "(abc def)"
    e(i("..,"), i("::=")) =? "(.., ::=)"
  }


  @Test
  def testNumExpr() : Unit = {
    n(123) =? "123"
    n(-123) =? "-123"
    n("12.3", 4) =? "12.3E4"
    n("12") =? "12."
    n("12.3", -4) =? "12.3E-4"
    n("-12.3", -4) =? "-12.3E-4"
    n("-12.3") =? "-12.3"
  }

  @Test
  def testStrings() : Unit = {
    s("abc") =? "\"abc\""
    s("abc\ndef\nghi") =? "\"abc\n  \n \"def\r    \r\n \r  \"ghi\""
  }

  @Test
  def testExpr1() : Unit = {
    e(i("alert"), e(i("<"), i("x"), "abc")) =? "(alert (< x \"abc\"))"
  }


  private def n(value : Int) : SExpression[BaseItem] =
    SLeaf(BaseInteger(BigInteger.valueOf(value)), empty)

  private def n(head : String, exp : Int) : SExpression[BaseItem] =
    SLeaf(BaseFloating(new BigDecimal(head), BigInteger.valueOf(exp)), empty)

  private def n(head : String) : SExpression[BaseItem] =
    n(head, 0)

  private def i(name : String) : SExpression[BaseItem] =
    SLeaf(BaseId(name), empty)

  implicit private def s(name : String) : SExpression[BaseItem] =
    SLeaf(BaseString(name), empty)


  private def e(items : SExpression[BaseItem]*) : SExpression[BaseItem] =
    SList(items, empty)


  private implicit class SEW(base : SExpression[BaseItem]) {
    @inline
    def =? (other : String) = {
      val parsed =
        SParser.parseSExpression(
          Function.const(empty))(Input.fromCharArray(other.toCharArray()))
      val pure = purify(parsed)
      assertEquals(base, pure)
    }


    private def purify(x : SExpression[BaseItem]) : SExpression[BaseItem] = {
      x match {
        case SLeaf(v, _) ⇒ SLeaf(v, empty)
        case SList(items, _) ⇒ SList(items.map(purify), empty)
      }
    }
  }
}

