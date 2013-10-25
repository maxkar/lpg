package ru.maxkar.lispy.parser

import ru.maxkar.lispy._
import ru.maxkar.lispy.parser._

import scala.collection.mutable.ArrayBuffer

import java.math._

/** S-expression parser. */
final object SParser {

  /** Reads a list of s-expressions. */
  private def readSList(
        attrParser : Input ⇒  Attributes,
        input : Input)
      : Seq[SExpression[BaseItem]] = {

    input.dropWhites

    val res = new ArrayBuffer[SExpression[BaseItem]]
    while (input.peek >= 0 && input.peek != ')') {
      res += parseSExpression(attrParser)(input)
      input.dropWhites
    }

    res.toSeq
  }

  /** Parses an s-expression using given
   * attributes/extensions parser.
   * @param attrParser extension parser.
   * @param input input to parse.
   */
  def parseSExpression
        (attrParser : Input ⇒ Attributes)
        (input : Input)
      : SExpression[BaseItem] = {
    input.dropWhites

    val start = input.location
    input.peek match {
      case x if x < 0 ⇒
        throw new BadSExpression(start)
      case '(' ⇒
        input.dropN(1)
        val headAtts = start ++ attrParser(input)
        val items = readSList(attrParser, input)
        if (input.peek != ')')
          throw new UnclosedSExpression(start, input.location)
        input.dropN(1)
        SList(items, headAtts ++ attrParser(input))
      case '-' if input.peekAt(1) > 0 && Character.isDigit(input.peekAt(1)) ⇒
        val item = readNum(input)
        SLeaf(item, start ++ attrParser(input))
      case x if Character.isDigit(x) ⇒
        val item = readNum(input)
        SLeaf(item, start ++ attrParser(input))
      case '"' ⇒
        input.dropN(1)
        val item = readString(start, input)
        SLeaf(item, start ++ attrParser(input))
      case x if isValidIdChar(x.asInstanceOf[Char]) ⇒
        val txt = input.charsWhile(isValidIdChar)
        SLeaf(BaseId(txt), start ++ attrParser(input))
      case _ =>
        throw new BadSExpression(start)
    }
  }


  /**
   * Parses enitre files as s-expression using given
   * attributes/extensions parser.
   * @param attrParser extension parser.
   * @param input input to parse.
   */
  def parseSFile
        (attrParser : Input ⇒ Attributes)
        (input : Input)
      : SList[BaseItem] = {
    input.dropWhites

    val start = attrParser(input)
    val items = readSList(attrParser, input)
    if (!input.atEof)
      throw new TrailingData(input.location)
    SList(items, start)
  }


  /** Reads an escaped character. */
  def readEsc(input : Input) : Char = {
    input.dropN(1)
    val la = input.peek
    input.dropN(1)
    la match {
      case 'r' ⇒ '\r'
      case 'n' ⇒ '\n'
      case 't' ⇒ '\t'
      case x ⇒
        if (x < 0)
          throw new BadEscapeChar(input.location)
        else
          x.asInstanceOf[Char]
    }
  }


  /** Reads a string. */
  def readString(start : Attributes, input : Input) : BaseItem = {
    val rb = new StringBuilder

    while (true) {
      rb ++= input.charsWhile(isCommonStringChar)

      input.peek match {
        case '\r' | '\n' ⇒
          rb += '\n'
          input.dropWhites
          if (input.peek != '"')
            throw new BadString(start, input.location)
          input.dropN(1)
        case '"' ⇒
          input.dropN(1)
          return new BaseString(rb.toString)
        case '\\' ⇒
          rb += readEsc(input)
        case _ ⇒
          throw new BadString(start, input.location)
      }
    }

    throw new AssertionError("Truth is broken!")
  }




  /** Reads a numeric item. */
  def readNum(input : Input) : BaseItem = {

    val hsign = input.peek match {
      case '-' ⇒
        input.dropN(1)
        "-"
      case _ ⇒  ""
    }
    val intp = readDigits(input)
    if (input.peek != '.')
      return new BaseInteger(new BigInteger(hsign + intp))

    input.dropN(1)
    val flop = readDigits(input)

    val fpn = new BigDecimal(hsign + intp + "." + flop)

    if (input.peek != 'e' && input.peek != 'E')
      return new BaseFloating(fpn, BigInteger.ZERO)

    input.dropN(1)
    val sgn = input.peek match {
      case '-' ⇒
        input.dropN(1)
        "-"
      case '+' ⇒
        input.dropN(1)
        ""
      case _ ⇒ ""
    }

    val exps = readDigits(input)
    if (exps.isEmpty)
      throw new BadExponent(input.location)

    new BaseFloating(fpn, new BigInteger(sgn + exps))
  }


  private def readDigits(input : Input) : String = {
    input.charsWhile(Character.isDigit)
  }


  private def isValidIdChar(char : Char) : Boolean = {
    if (Character.isJavaIdentifierPart(char))
      return true
    char match {
      case '~' | '!' | '@' | '#' | '$' | '%' | '^' | '&' |
        '*' | '-' | '_' | '+' | '=' | '/' | '|' | ':' | '.' | ',' |
        '<' | '>' ⇒  true
      case _ ⇒  false
    }
  }


  private def isCommonStringChar(char : Char) : Boolean = {
    char match {
      case '\\' | '"' | '\r' | '\n' ⇒ false
      case _ ⇒  true
    }
  }
}
