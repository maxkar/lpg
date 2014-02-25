package ru.maxkar.backend.js.out.syn

import scala.language.implicitConversions
import java.math.BigInteger
import java.math.BigDecimal
import scala.collection.mutable.ArrayBuffer

/**
 * Compact syntax writer. Tries to write code as tight as possible.
 * This means no whitespaces, no indentation.
 * @param F type of the output.
 */
final class CompactSyntax[F](
      writer : CompactFragmenter[F]
    ) extends
    Syntax[String, String, CompactExpression[F], CompactStatement[F]] {

  /** Synonim for the expression type. */
  private type E = CompactExpression[F]

  /** Synonim for the statement type. */
  private type S = CompactStatement[F]



  /* Trait implementation. */

  override val exprUndefined : E = textExpr("undefined")

  override val exprNull : E = textExpr("null")

  override val exprArguments : E = textExpr("arguments")

  override def literal(value : String) : E =
    textExpr(quoteString(value))

  override def literal(value : BigInteger) : E =
    textExpr(value.toString())

  override def literal(value : BigDecimal, exp : BigInteger) : E =
    textExpr(value + "E" + exp)

  override def literal(value : Boolean) : E =
    textExpr(if (value) "true" else "false")

  override def arrayLiteral(items : Seq[E]) : E =
    exprn(0, seq("[", sepBy(",", items map commaSafe), "]"))

  override def objectLiteral(values : Seq[(String, E)]) : E =
    exprn(0, seq("{", sepBy(",", values map mapEntry), "}"),
      statementStart = false)

  override def variable(name : String) : E = exprn(0, name)

  override def functionExpr(
        name : Option[String],
        args : Seq[String],
        body : Seq[S])
      : E =
    exprn(0, seq(
        "function",
        name match {
          case None ⇒ ""
          case Some(x) ⇒ " " + x
        }, "(",
        sepBy(",", args map str2writer), "){",
        body.map(x ⇒ x.writer), "}"),
      statementStart = false)

  override def knownMember(base : E, member : String) : E =
    if (isValidSimpleId(member)) {
      val brackets = base.priority > 0
      exprn(0, seq(
          bracketed(brackets, "(", ")", base),
          ".", member),
        statementStart = brackets || base.statementStart,
        minusSafe = brackets || base.minusSafe)
    } else
      dynamicMember(base, literal(member))

  override def dynamicMember(base : E, member : E) : E = {
    val brackets = base.priority > 0
    exprn(0, seq(
        bracketed(brackets, "(", ")", base),
        "[", member.writer, "]"),
      statementStart = brackets || base.statementStart,
      minusSafe = brackets || base.minusSafe)
  }

  override def create(specifier : E, args : Seq[E]) : E =
    exprn(1, seq(
        if (specifier.priority > 1)
          seq("new(", specifier, ")")
        else
          seq("new ", specifier)
        ,
        "(", sepBy(",", args map commaSafe), ")"))

  override def call(specifier : E, args : Seq[E]) : E = {
    val fnbrackets = specifier.priority > 2
    exprn(2, seq(
        bracketed(fnbrackets, "(", ")", specifier),
        "(", args map commaSafe, ")"),
      statementStart = fnbrackets || specifier.statementStart,
      minusSafe = fnbrackets || specifier.minusSafe)
  }

  override def prefixInc(base : E) : E =
    prefixOp(base, "++")

  override def prefixDec(base : E) : E =
    prefixOp(base, "--")

  override def postfixInc(base : E) : E =
    postfixOp(base, "++")

  override def postfixDec(base : E) : E =
    postfixOp(base, "--")

  override def bitNot(base : E) : E =
    unaryOp(base, "~")

  override def boolNot(base : E) : E =
    unaryOp(base, "!")

  override def neg(base : E) : E = {
    val negPriority = 4
    val writer =
      if (base.priority > negPriority)
        seq("-(", base, ")")
      else if (!base.minusSafe)
        seq("- ", base)
      else
        seq("-", base)
    exprn(negPriority, writer, minusSafe = false)
  }

  override def typeOf(base : E) : E =
    kwdUnaryOp(base, "typeof")

  override def voidOf(base : E) : E =
    kwdUnaryOp(base, "void")

  override def delete(base : E) : E =
    kwdUnaryOp(base, "delete")

  override def mul(left : E, right : E) : E =
    binaryOp(left, right, "*", 5)

  override def div(left : E, right : E) : E =
    binaryOp(left, right, "/", 5)

  override def rem(left : E, right : E) : E =
    binaryOp(left, right, "%", 5)

  override def add(left : E, right : E) : E =
    binaryOp(left, right, "+", 6)

  override def sub(left : E, right : E) : E = {
    val priority = 6
    val lbracket = left.priority > priority
    val rbracket = right.priority >= priority
    val rwriter =
      if (rbracket)
        bracketed("(", ")", right)
      else if (!right.minusSafe)
        seq(" ", right)
      else
        right.writer
    exprn(priority, seq(
        bracketed(lbracket, "(", ")", left),
        "-", rwriter),
      minusSafe = lbracket || left.minusSafe,
      statementStart = lbracket || left.statementStart)
  }

  override def shl(value : E, shift : E) : E =
    binaryOp(value, shift, "<<", 7)

  override def sshr(value : E, shift : E) : E =
    binaryOp(value, shift, ">>", 7)

  override def ushr(value : E, shift : E) : E =
    binaryOp(value, shift, ">>>", 7)

  override def less(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "<", 8)

  override def lessEq(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "<=", 8)

  override def greater(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, ">", 8)

  override def greaterEq(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, ">=", 8)

  override def isIn(expr1 : E, expr2 : E) : E =
    binaryKwdOp(expr1, expr2, "in", 8)

  override def isInstanceOf(expr1 : E, expr2 : E) : E =
    binaryKwdOp(expr1, expr2, "instanceof", 8)

  override def equals(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "==", 9)

  override def notEquals(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "!=", 9)

  override def strictEquals(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "===", 9)

  override def strictNotEquals(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "!==", 9)

  override def bitAnd(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "&", 10)

  override def bitXor(expr1 : E, expr2 : E) : E =
    binaryOp(expr1, expr2, "^", 11)

  override def bitOr(left : E, right : E) : E =
    binaryOp(left, right, "|", 12)

  override def boolAnd(left : E, right : E) : E =
    binaryOp(left, right, "&&", 13)

  override def boolOr(left : E, right : E) : E =
    binaryOp(left, right, "||", 14)

  override def condExpr(cond : E, onTrue : E, onFalse : E) : E = {
    val priority = 15
    val lbracket = cond.priority >= priority
    val rbracket = onFalse.priority > priority
    exprn(priority, seq(
        bracketed(lbracket, "(", ")", cond),
        "?", onTrue, ":",
        bracketed(rbracket, "(", ")", onFalse)),
      minusSafe = lbracket || cond.minusSafe,
      statementStart = lbracket || cond.statementStart)
  }

  override def assign(base : E, value : E) : E =
    binaryOp(base, value, "=", 17)

  override def inplaceAdd(base : E, value : E) : E =
    binaryOp(base, value, "+=", 17)

  override def inplaceSub(base : E, value : E) : E =
    binaryOp(base, value, "-=", 17)

  override def inplaceMul(base : E, value : E) : E =
    binaryOp(base, value, "*=", 17)

  override def inplaceDiv(base : E, value : E) : E =
    binaryOp(base, value, "/=", 17)

  override def inplaceRem(base : E, value : E) : E =
    binaryOp(base, value, "%=", 17)

  override def inplaceShl(base : E, shift : E) : E =
    binaryOp(base, shift, "<<=", 17)

  override def inplaceSshr(base : E, shift : E) : E =
    binaryOp(base, shift, ">>=", 17)

  override def inplaceUshr(base : E, shift : E) : E =
    binaryOp(base, shift, ">>>=", 17)

  override def inplaceBitAnd(base : E, value : E) : E =
    binaryOp(base, value, "&=", 17)

  override def inplaceBitXor(base : E, value : E) : E =
    binaryOp(base, value, "^=", 17)

  override def inplaceBitOr(base : E, value : E) : E =
    binaryOp(base, value, "|=", 17)

  override def seqExpr(head : E, tail : Seq[E]) : E =
    exprn(18, sepBy(",", (head +: tail) map expr2writer),
      minusSafe = head.minusSafe,
      commaSafe = false,
      statementStart = head.statementStart)

  override def exprStatement(expr : E) : S =
    if (expr.statementStart)
      stmt(seq(expr.writer, ";"))
    else
      stmt(seq("(", expr.writer, ");"))

  override val breakOuter : S = stmt("break;")

  override def breakLabeled(label : String) : S =
    stmt(seq("break ", label, ";"))

  override val continueOuter : S = stmt("continue;")

  override def continueLabeled(label : String) : S =
    stmt(seq("continue ", label, ";"))

  override def doWhile(body : Seq[S], cond : E) : S =
    stmt(seq(
        "do{", seq2writer(body map stmt2writer),
        "}while(", cond, ");"))

  override def forVars(
        variables : Seq[(String, Option[E])],
        condition : Option[E],
        finallyExpression : Option[E],
        body : Seq[S])
      : S =
    stmt(seq(
        "for(",
        if (variables.isEmpty)
          str2writer("")
        else
          seq("var ", varDecls(variables)),
        ";", unoptExpr(condition),
        ";", unoptExpr(finallyExpression), ")",
        asSingleStmt(body)))

  override def forExisting(
        init : Option[E],
        condition : Option[E],
        finallyExpression : Option[E],
        body : Seq[S])
      : S =
    stmt(seq(
        "for(",
        unoptExpr(init),
        ";", unoptExpr(condition),
        ";", unoptExpr(finallyExpression), ")",
        asSingleStmt(body)))

  override def forVariableIn(variable : String, collection : E, body : Seq[S]) : S =
    stmt(seq(
        "for(var ", variable, " in ", collection, ")",
        asSingleStmt(body)))

  override def forExistingIn(iterator : E, collection : E, body : Seq[S]) : S =
    stmt(seq(
        "for(", iterator, " in ", collection, ")",
        asSingleStmt(body)))

  override def functionStmt(name : Option[String], args : Seq[String], body : Seq[S]) : S =
    stmt(seq(
        "function",
        name match {
          case None ⇒ ""
          case Some(x) ⇒ " " + x
        }, "(",
        sepBy(",", args map str2writer), "){",
        body.map(x ⇒ x.writer), "}"))

  override def ifStatement(cond : E, onTrue : Seq[S], onFalse : Seq[S]) : S =
    stmt(seq(
        "if(", cond, ")",
        asSingleStmt(onTrue),
        if (onFalse.length == 1) "else " else "else",
        asSingleStmt(onFalse)))

  override def label(name : String, body : Seq[S]) : S =
    stmt(seq(name, ":", asSingleStmt(body)))

  override val retNone : S = stmt("return;")

  override def ret(value : E) : S =
    stmt(seq("return ", value, ";"))

  override def chooseValue(
        discriminator : E,
        cases : Seq[(E, Seq[S])],
        onElse : Seq[S])
      : S =
    stmt(seq(
        "switch(", discriminator, "){",
        cases map (cond ⇒ seq(
          "case ", cond._1, ":", cond._2 map stmt2writer)),
        if (onElse.isEmpty) "" else seq("default:", onElse.map(stmt2writer)),
        "}"))

  override def raise(value : E) : S =
    stmt(seq("throw ", value))

  override def tryCatch(
        body : Seq[S],
        onException : Option[(String, Seq[S])],
        finalizer : Seq[S])
      : S = {
    if (!onException.isDefined && finalizer.isEmpty)
      return stmt(asSingleStmt(body))
    stmt(seq(
        "try{", body map stmt2writer,
        onException match {
          case None ⇒ ""
          case Some((v, handler)) ⇒
            seq("}catch(", v, "){", handler map stmt2writer, "}")
          },
        if (finalizer.isEmpty)
          ""
        else
          seq("finally{", finalizer map stmt2writer, "}")))
  }

  override def defVars(definitions: Seq[(String, Option[E])]) : S =
    if (definitions.isEmpty)
      stmt("")
    else
      stmt(seq(
          "var ", varDecls(definitions), ";"))

  override def whileDo(cond : E, body : Seq[S]) : S =
    stmt(seq(
        "while(", cond, ")", asSingleStmt(body)))



  /* Factories. */

  /** Plain (base) expression. */
  private def textExpr(text : String) : E =
    exprn(0, text, commaSafe = !text.startsWith("-"))

  /** Quotes an input string. */
  def quoteString(expr : String) : String = {
    val buf = new StringBuilder(expr.length + 2)
    buf += '"'

    var ptr = 0
    while (ptr < expr.length) {
      expr.charAt(ptr) match {
        case '\\' ⇒ buf ++= "\\\\"
        case '\r' ⇒ buf ++= "\\r"
        case '\n' ⇒ buf ++= "\\n"
        case '\t' ⇒ buf ++= "\\t"
        case '\b' ⇒ buf ++= "\\b"
        case '\"' ⇒ buf ++= "\\\""
        case x ⇒ buf += x
      }
      ptr += 1
    }

    buf += '"'
    buf.toString
  }

  /** Writes item with separator. */
  private def sepBy(separator : F, items : Iterable[F]) : F =
    seq(intersperse(separator, items) :_*)

  /** Writes an expression as comma-safe. */
  private def commaSafe(expr : E) : F =
    bracketed(!expr.commaSafe, "(", ")", expr.writer)

  /** Creates a possibly-bracketed expression. */
  private def bracketed(lbracket : F, rbracket : F, content : F) : F =
    seq(lbracket, content, rbracket)

  /** Optionally bracketed items. */
  private def bracketed(
        cond : Boolean,
        lbracket : F,
        rbracket : F,
        content : F)
      : F =
    if (cond) bracketed(lbracket, rbracket, content) else content

  /** Expression factory. */
  @inline
  private def exprn(
        priority : Int,
        w : F,
        commaSafe : Boolean = true,
        minusSafe : Boolean = true,
        statementStart : Boolean = true)
      : CompactExpression[F] =
    new CompactExpression(w, priority, commaSafe, minusSafe, statementStart)

  /** Writer for the option. */
  private def unopt(x : Option[String]) : F =
    x match {
      case None ⇒ ""
      case Some(x) ⇒ x
    }

  /** Option extractor for expression. */
  private def unoptExpr(x : Option[E]) : F =
    x match {
      case None ⇒ ""
      case Some(x) ⇒ x
    }

  /** Writer for map entry. */
  private def mapEntry(entry : (String, E)) : F =
    seq(
      if (isValidSimpleId(entry._1)) entry._1 else quoteString(entry._1),
    entry._2.writer)

  /** Prefix operation factory. */
  private def prefixOp(base : E, op : String) : E = {
    val brackets = base.priority >= 2
    exprn(3, seq(op, bracketed(brackets, "(", ")", base)),
      minusSafe = !op.startsWith("-"))
  }

  /** Postfix operation writer. */
  private def postfixOp(base : E, op : String) : E = {
    val brackets = base.priority >= 2
    exprn(3, seq(bracketed(brackets, "(", ")", base), op),
      statementStart = brackets || base.statementStart)
  }

  /** Unary operator factory. */
  private def unaryOp(base : E, op : String) : E =
    exprn(4, seq(op, bracketed(base.priority > 4, "(", ")", base)),
      minusSafe = !op.startsWith("-"))

  /** Unary operation with a keyword (not symbol) name. */
  private def kwdUnaryOp(base : E, op : String) : E = {
    val writer =
      if (base.priority > 4)
        seq(op, "(", base, ")")
      else
        seq(op, " ", base)
    exprn(4, writer)
  }

  /** Binary operation writer. */
  private def binaryOp(left : E, right : E, op : String, priority : Int) : E = {
    val lbracket = left.priority > priority
    val rbracket = right.priority >= priority
    exprn(priority, seq(
        bracketed(lbracket, "(", ")", left), op,
        bracketed(rbracket, "(", ")", right)),
      statementStart = lbracket || left.statementStart)
  }

  /** Binary operation denoted by the word. */
  private def binaryKwdOp(left : E, right : E, op : String, priority : Int) : E =
    binaryOp(left, right, " " + op + " ", priority)

  /** Statement creator synonim. */
  private def stmt(writer : F) : S =
    new CompactStatement[F](writer)

  /** Variable declaration writer. */
  private def varDecl(declaration : (String, Option[E])) : F =
    declaration._2 match {
      case None ⇒ declaration._1
      case Some(x) ⇒ seq(declaration._1, "=", commaSafe(x))
    }

  /** Variables declaration. */
  private def varDecls(declarations : Seq[(String, Option[E])]) : F =
    sepBy(",", declarations map varDecl)

  /** Converts list of statements to one statement. */
  private def asSingleStmt(statements : Seq[S]) : F =
    if (statements.length == 1)
      statements.head
    else
      seq("{", seq2writer(statements map stmt2writer), "}")



  /* Utilities. */
  /** Sequence writer. */
  @inline
  private def seq(items : F*) : F =
    writer.compose(items)

  /** Conversion form string to writer. */
  @inline
  private implicit def str2writer(str : String) : F =
    writer.fromString(str)

  /** Conversion from writers sequence to writer fragment. */
  @inline
  private implicit def seq2writer(items : Seq[F]) : F =
    writer.compose(items)

  /** Conversion from expression to writer. */
  @inline
  private implicit def expr2writer(expr : E) : F = expr.writer

  /** Conversion form statement to writer. */
  @inline
  private implicit def stmt2writer(stmt : S) : F = stmt.writer

  /**
   * Interperses element between elements of the iterable.
   * For example, interperse(1, [a,b,c]) = [a,1,b,1,c,1]
   */
  private def intersperse[T](item : T, items : Iterable[T]) : Seq[T] = {
    val itr = items.iterator
    if (!itr.hasNext)
      return Seq.empty

    val res = new ArrayBuffer[T]
    res += itr.next()

    while (itr.hasNext) {
      res += item
      res += itr.next()
    }

    return res
  }

  /** Checks, if value is valid identifier. */
  private def isValidSimpleId(value :String) : Boolean = {
    if (value.isEmpty)
      return false
    if (Character.isDigit(value.charAt(0)))
      return false

    var ptr = 0
    while (ptr < value.length) {
      if (!isValidIdentifierChar(value.charAt(ptr)))
        return false
      ptr+=1
    }

    true
  }


  /** Checks, if character is valid identifier char. */
  private def isValidIdentifierChar(c : Char) : Boolean = {
    Character.getType(c) match {
      case
        Character.UPPERCASE_LETTER |
        Character.LOWERCASE_LETTER |
        Character.TITLECASE_LETTER |
        Character.MODIFIER_LETTER |
        Character.OTHER_LETTER |
        Character.LETTER_NUMBER ⇒ true
      case _ ⇒ false
    }
  }
}
