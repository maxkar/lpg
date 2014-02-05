package ru.maxkar.backend.js.model

import java.math.BigInteger
import java.math.BigDecimal

import ru.maxkar.backend.js.out.CompactContext
import ru.maxkar.backend.js.out.writer.Writer
import ru.maxkar.backend.js.out.writer.ContextWriter

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

/** Model element factories.
 * Lacks support of regular expressions at this point.
 */
final object Model {

  private[model] type OutFragment = CompactContext ⇒ Unit

  /** Output typeclass. */
  private[model] val outClass : Writer[OutFragment] =
    new ContextWriter((s, ctx) ⇒  ctx.write(s))


  /** Failure expression. */
  val failure : LeftValue =
    new LeftValue(0, ctx ⇒
        throw new IllegalStateException("Cannot write failure!"))


  /** Boolean "true" expression. */
  val exprTrue : Expression = textExpr("true")


  /** Boolean "false" expression. */
  val exprFalse : Expression = textExpr("false")


  /** Null expression. */
  val exprNull : Expression = textExpr("null")


  /** Undefined expression. */
  val undefined : Expression = textExpr("undefined")


  /** Function arguments expression. */
  val arguments : Expression = textExpr("arguments")


  /** Creates a function definition. */
  def defun(id : AnyRef, args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : (AnyRef, FunctionBody) =
    (id, mkFunctionBody(args, locals, localFuncs, labels, body))


  /** Creates a string expression. */
  def literal(expr : String) : Expression = {
    val memberAccessor =
      if (validSimpleId(expr)) Some(expr) else None
    val quoted = quoteString(expr)
    new Expression(0, quoted, simpleMemberAccessor = memberAccessor)
  }


  /** Creates a number literal expression. */
  def literal(value : Int) : Expression =
    textExpr(value.toString)


  /** Creates a numeric literal expression. */
  def literal(value : Long) : Expression =
    textExpr(value.toString)


  /** Creates a numeric literal experssion. */
  def literal(value : BigInteger) : Expression =
    textExpr(value.toString)


  /** Creates a number literal expression. */
  def literal(value : Double) : Expression =
    textExpr(value.toString)


  /** Creates a number literal expression. */
  def literal(head : BigDecimal, tail : BigInteger) : Expression =
    textExpr(head.toString + "E" + tail.toString)


  /** Creates a boolean literal. */
  def literal(value : Boolean) : Expression =
    if (value) exprTrue else exprFalse


  /** Creates an array expression. */
  def arrayliteral(elts : Expression*) : Expression =
    new Expression(0, outClass.seq(
      "[", sepBy(",", elts.map(commaSafe)), "]"))


  /** Creates an object literal. */
  def objectliteral(elts : (String, Expression)*) : Expression =
    new Expression(0, outClass.seq(
        "{", sepBy(",", elts.map(writerForMapEntry)), "}"
      ), canStartStatement = false)


  /**
   * Creates writer for the map entry.
   */
  private def writerForMapEntry(entry : (String, Expression)) : OutFragment =
    outClass.seq(
      if (validSimpleId(entry._1))
        outClass.token(entry._1)
      else
        outClass.token(quoteString(entry._1)),
      ":", commaSafe(entry._2)
    )


  /** Creates a reference to a variable in outer scope. */
  def variable(ref : AnyRef) : LeftValue =
    new LeftValue(0, varWriter(ref))


  /** Creates an anonymous local function. */
  def anonfun(args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression = {

    val fb = mkFunctionBody(args, locals, localFuncs, labels, body)
    new Expression(0,
      outClass.seq("function", fb.writer),
      canStartStatement = false)
  }


  /** Creates a named (self-referentiable) ocal function. */
  def namedfun(id : AnyRef, args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression = {
    val fb = mkFunctionBody(args, locals, localFuncs, labels, body)
    new Expression(0,
      subFunction(Set(id), labels,
        outClass.seq("function ", varWriter(id), fb.writer)),
      canStartStatement = false)
  }


  /** Member access expression. */
  def member(base : Expression, item : Expression)
      : LeftValue = {
    val basebrackets = base.priority > 0
    new LeftValue(0,
      outClass.seq(
        bracketed(basebrackets, "(", ")", base.writer),
        memberAccessorWriter(item)),
      canStartStatement =  base.canStartStatement || basebrackets)
  }

  /** Creates a new instance creation expression. */
  def create(base : NonprimitiveExpression, args : Expression*)
      : NonprimitiveExpression =
    new NonprimitiveExpression(1, outClass.seq(
        "new ",
        bracketed(base.priority > 1, "(", ")", base.writer),
        "(", sepBy(",", args.map(commaSafe)), ")"
      ))


  /** Writes a function call. */
  def call(base : NonprimitiveExpression, args : Expression*)
      : NonprimitiveExpression = {
    val fnbrackets = base.priority > 2
    new NonprimitiveExpression(2, outClass.seq(
        bracketed(fnbrackets, "(", ")", base.writer),
        "(", sepBy(",", args.map(commaSafe)), ")"
      ), canStartStatement = base.canStartStatement || fnbrackets)
  }


  /** Prefix increment expression. */
  def prefixInc(base : LeftValue) : Expression =
    prefixOp(base, "++")


  /** Prefix decrement expression. */
  def prefixDec(base : LeftValue) : Expression =
    prefixOp(base, "--")


  /** Postfix increment expression. */
  def postfixInc(base : LeftValue) : Expression =
    postfixOp(base, "++")


  /** Postfix decrement expression. */
  def postfixDec(base : LeftValue) : Expression =
    postfixOp(base, "--")


  /** Logical not. */
  def boolNot(base : Expression) : Expression =
    unary(base, "!")


  /** Bitwise not. */
  def bitNot(base : Expression) : Expression =
    unary(base, "~")


  /** Negation expression. */
  def neg(base : Expression) : Expression = {
    val negPriority = 4
    val resWriter =
      if (base.priority > negPriority)
        bracketed(true, "(", ")", base.writer)
      else if (base.isMinusSafe)
        base.writer
      else
        outClass.seq(" ", base.writer)

    new Expression(negPriority, outClass.seq("-", resWriter),
      isMinusSafe = false)
  }


  /** "Type of" expression. */
  def typeof(base : Expression) : Expression =
    unary(base, "typeof ")


  /** "Void" expression. */
  def voidof(base : Expression) : Expression =
    unary(base, "void ")


  /** "Delete" expression. */
  def delete(base : NonprimitiveExpression) : Expression =
    unary(base, "delete ")


  /** Multiplication expression. */
  def mul(left : Expression, right : Expression) : Expression =
    binary(left, right, "*", 5)


  /** Division expression. */
  def div(left : Expression, right : Expression) : Expression =
    binary(left, right, "/", 5)


  /** Remainder expression. */
  def rem(left : Expression, right : Expression) : Expression =
    binary(left, right, "%", 5)


  /** Addition expression. */
  def add(left : Expression, right : Expression) : Expression =
    binary(left, right, "+", 6)


  /** Subtraction expression. */
  def sub(left : Expression, right : Expression) : Expression = {
    val subPriority = 6
    val lbracket = left.priority > subPriority
    val rbracket = right.priority >= subPriority
    val rightWriter =
      if (right.priority >= subPriority)
        bracketed(true, "(", ")", right.writer)
      else if (right.isMinusSafe)
        right.writer
      else
        outClass.seq(" ", right.writer)

    new Expression(subPriority,
      outClass.seq(
        bracketed(lbracket, "(", ")", left.writer),
        "-",
        rightWriter),
      canStartStatement = left.canStartStatement || lbracket)
  }


  /** Shift-left expression. */
  def shl(left : Expression, right : Expression) : Expression =
    binary(left, right, "<<", 7)


  /** Signed shift right expression. */
  def sshr(left : Expression, right : Expression) : Expression =
    binary(left, right, ">>", 7)


  /** Unsighed shift right expression. */
  def ushr(left : Expression, right : Expression) : Expression =
    binary(left, right, ">>>", 7)


  /** Less comparison expression. */
  def less(left : Expression, right : Expression) : Expression =
    binary(left, right, "<", 8)


  /** Less or equals comparison expression. */
  def lessEq(left : Expression, right : Expression) : Expression =
    binary(left, right, "<=", 8)


  /** Greater comparison expression. */
  def greater(left : Expression, right : Expression) : Expression =
    binary(left, right, ">", 8)


  /** Greator or equals expression. */
  def greaterEq(left : Expression, right : Expression) : Expression =
    binary(left, right, ">=", 8)


  /** "In" check expression. */
  def isIn(left : Expression, right : Expression) : Expression =
    binary(left, right, " in ", 8)


  /** Conditional cast expression. */
  def testInstanceOf(left : Expression, right : Expression) : Expression =
    binary(left, right, " instanceof ", 8)


  /** Equals expression. */
  def equalsTo(left : Expression, right : Expression) : Expression =
    binary(left, right, "==", 9)


  /** Not equals expression. */
  def notEquals(left : Expression, right : Expression) : Expression =
    binary(left, right, "!=", 9)


  /** Strict equality comparison. */
  def strictEquals(left : Expression, right : Expression) : Expression =
    binary(left, right, "===", 9)


  /** String non-equals expression. */
  def strictNotEquals(left : Expression, right : Expression) : Expression =
    binary(left, right, "!==", 9)


  /** Bitwise and expression. */
  def bitAnd(left : Expression, right : Expression) : Expression =
    binary(left, right, "&", 10)


  /** Bitwise exclusive or expression. */
  def bitXor(left : Expression, right : Expression) : Expression =
    binary(left, right, "^", 11)


  /** Bitwise or expression. */
  def bitOr(left : Expression, right : Expression) : Expression =
    binary(left, right, "|", 12)


  /** Boolean and expression. */
  def boolAnd(left : Expression, right : Expression) : Expression =
    binary(left, right, "&&", 13)


  /** Boolean or expression. */
  def boolOr(left : Expression, right : Expression) : Expression =
    binary(left, right, "||", 14)


  /** Conditional expression. */
  def cond(cond : Expression,
      onTrue : Expression, onFalse: Expression) : Expression = {
    val priority = 15
    val lbracket = cond.priority >= priority
    val rbracket = onFalse.priority > priority

    new Expression(priority, outClass.seq(
        bracketed(lbracket, "(", ")", cond.writer),
        "?", onTrue.writer, ":",
        bracketed(rbracket, "(", ")", onFalse.writer)
      ),
      canStartStatement = cond.canStartStatement || lbracket)
  }


  /** Assignment expression. */
  def assign(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "=",17)


  /** Inplace addition expression. */
  def inplaceAdd(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "+=",17)


  /** Inplace subtraction expression. */
  def inplaceSub(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "-=",17)


  /** Inplace multiplication expression. */
  def inplaceMul(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "*=",17)


  /** Inplace division expression. */
  def inplaceDiv(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "/=",17)


  /** Inplace remainder expression. */
  def inplaceRem(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "%=",17)


  /** Inplace shilt left expression. */
  def inplaceShl(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "<<=",17)


  /** Inplace signed shift right expression. */
  def inplaceSshr(host : LeftValue, value : Expression) : Expression =
    binary(host, value, ">>=",17)


  /** Inplace unsigned shift right expression. */
  def inplaceUshr(host : LeftValue, value : Expression) : Expression =
    binary(host, value, ">>>=",17)


  /** Inplace bitwise and expression. */
  def inplaceBitAnd(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "&=",17)


  /** Inplace bitwise or expression. */
  def inplaceBitOr(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "|=",17)


  /** Inplace bitwise exclusive or expression. */
  def inplaceBitXor(host : LeftValue, value : Expression) : Expression =
    binary(host, value, "^=",17)


  /** Sequence (comma) expression. */
  def seqExpr(first : Expression, second : Expression) : Expression =
    new Expression(18,
      outClass.seq(first.writer, ",", second.writer),
      isCommaSafe = false,
      canStartStatement = first.canStartStatement)

  /** Converts expression into statement. */
  implicit def expr2statement(expr : Expression) : Statement =
    new Statement(outClass.seq(
        Model.bracketed(
          !expr.canStartStatement, "(", ")", expr.writer),
        Model.outClass.token(";")))


  /** Breaks from an outer statement. */
  def breakOuter() : Statement = textStmt("break")


  /** Breaks a labeled statement. */
  def breakL(label : AnyRef) = labeledStmt("break", label)


  /** Continues an outer statement. */
  def continueOuter() : Statement = textStmt("continue")


  /** Continues a labeled statement. */
  def continueL(label : AnyRef) = labeledStmt("continue", label)


  /** Preforms statement with a postfix condition check. */
  def doWhile(items : Seq[Statement], cond : Expression) : Statement =
    new Statement(outClass.seq(
      "do{",
      outClass.seq(items.map(_.stmtWriter) :_*),
      "}while(", cond.writer, ");"
    ))


  /** Creates a for statement. */
  def whileWithIterupdate(cond : Expression, update : Expression,
      body : Seq[Statement]) : Statement =
    new Statement(outClass.seq(
      "for(;", cond.writer,";",update.writer, ")",
      bracketed(body.size != 1, "{", "}",
        outClass.seq(body.map(_.stmtWriter) : _*))
    ))


  /** Performs iteration over container keys. */
  def forIn(iter : LeftValue, collection : Expression,
      body : Seq[Statement]) : Statement =
    new Statement(outClass.seq(
      "for (", iter.writer, " in ", collection.writer, ")",
      bracketed(body.size != 1, "{", "}",
        outClass.seq(body.map(_.stmtWriter) :_*))
    ))

  /** Performs statements when condition is true. */
  def when(cond : Expression, body : Seq[Statement]) : Statement =
    doCond(cond, body, Seq.empty)


  /** Chooses one of two statements. */
  def doCond(condition : Expression, onTrue : Seq[Statement],
      onFalse : Seq[Statement]): Statement =
    new Statement(outClass.seq(
      "if(", condition.writer, ")",
      bracketed(onTrue.size != 1, "{", "}",
        outClass.seq(onTrue.map(_.stmtWriter) : _*)),
      if (onFalse.size != 1) "else" else "else ",
      bracketed(onFalse.size != 1, "{", "}",
        outClass.seq(onFalse.map(_.stmtWriter) : _*))
    ))


  /** Labels a statement. */
  def label(lbl : AnyRef, body : Statement) : Statement =
    new Statement(outClass.seq(
      labelWriter(lbl), ":", body.stmtWriter
    ))


  /** Return statement. */
  def returns(value : Expression) : Statement =
    kwdStmt("return", value)


  /** Return-nothing statement. */
  def returnNothing() : Statement =
    textStmt("return")


  /** Switch statement. */
  def switchof(cond : Expression,
      rmap : Seq[(Seq[Expression], Seq[Statement])],
      onElse : Option[Seq[Statement]]) : Statement = {

    val fragments = new ArrayBuffer[OutFragment]
    fragments +=
      "switch(" += cond.writer += "){"

    for ((ks, ss) ← rmap)
      if (!ks.isEmpty) {
        ks.foreach(k ⇒
          fragments += "case " += k.writer += ":")
        fragments ++= ss.map(_.stmtWriter)
        fragments += "break;"
      }

    onElse match {
      case None ⇒ ()
      case Some(x) ⇒
        fragments += "default:"
        fragments ++= x.map(_.stmtWriter)
    }

    fragments += "}"

    new Statement(outClass.seq(fragments :_*))
  }


  /** Throw statement. */
  def throws(value : Expression) : Statement =
    kwdStmt("throw", value)


  /** Try/catch statement. */
  def tryCatch( body : Seq[Statement],
      exnId : AnyRef,
      exnHandler : Seq[Statement]) : Statement =
    new Statement(outClass.seq(
      "try{", outClass.seq(body.map(_.stmtWriter) :_*), "}catch(",
      subContext(Set(exnId), outClass.seq(
        varWriter(exnId),
        "){", outClass.seq(exnHandler.map(_.stmtWriter) : _*),
        "}"))
    ))


  /** Tries with finalizer. */
  def withFin(body : Seq[Statement], fin : Seq[Statement]) : Statement =
    new Statement(outClass.seq(
      "try{", outClass.seq(body.map(_.stmtWriter) :_*),
      "}finally{", outClass.seq(fin.map(_.stmtWriter) :_*),
      "}"
    ))


  /** While statement. */
  def whiles(cond : Expression, body : Seq[Statement]) : Statement =
    new Statement(outClass.seq(
      "while(", cond.writer, ")",
      bracketed(body.size != 1, "{", "}",
        outClass.seq(body.map(_.stmtWriter) : _*))
    ))


  /** Creates a javascript file. */
  def file(
        globals : Seq[(AnyRef, String)] = Seq.empty,
        vars : Seq[AnyRef] = Seq.empty,
        funcs :Seq[(AnyRef, FunctionBody)] = Seq.empty,
        inits : Seq[Statement] = Seq.empty) :
      JSFile = {

    val uniqueNames = globals.map(_._2).toSet
    if (uniqueNames.size != globals.size)
      throw new IllegalArgumentException("Non-unique global name present")

    val localIds = (vars ++ funcs.map(_._1)).toSet -- globals.map(_._1).toSet

    val operations = new ArrayBuffer[CompactContext ⇒  Unit]

    if (!vars.isEmpty)
      operations += "var " += sepBy(",", vars.map(varWriter)) += ";"

    funcs.foreach(x ⇒
      operations += "function " += varWriter(x._1) += x._2.writer)

    operations ++= inits.map(x ⇒ x.writeStatement _)

    new JSFile(globals.toMap, outClass.seq(operations : _*))
  }


  /**
   * Outputs js file into a given writer.
   * @param file file to write.
   * @param w target stream.
   */
  def writeFileToWriter(file : JSFile, w : java.io.Writer) : Unit = {
    val ctx = CompactContext.forWriter(w, file.globals)
    file.writer(ctx)
  }



  /** Quotes an input string. */
  private[model] def quoteString(expr : String) : String = {
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


  /** Creates a plaintext expression. */
  private def textExpr(value : String) : Expression =
    new Expression(0, value, isMinusSafe = !value.startsWith("-"))


  /** Creates a prefix operation. */
  private def prefixOp(peer : LeftValue, op : String) : Expression =
    new Expression(2, outClass.seq(op, peer.writer),
      isMinusSafe = !op.startsWith("-"))


  /** Creates a postfix operation. */
  private def postfixOp(peer : LeftValue, op : String) : Expression =
    new Expression(2, outClass.seq(peer.writer, op),
      canStartStatement = peer.canStartStatement)


  /** Creates a new unary expression. */
  private def unary(base : Expression, code : String) : Expression =
    new Expression(4,
      outClass.seq(code, bracketed(base.priority > 4, "(", ")", base.writer)))


  /** Creates a binary expression. */
  private def binary(
      left : Expression, right : Expression, sign : String,
      bpriority : Int) : Expression = {
    val lbracket = left.priority > bpriority
    val rbracket = right.priority >= bpriority
    new Expression(bpriority, outClass.seq(
        bracketed(lbracket, "(", ")", left.writer),
        sign,
        bracketed(rbracket, "(", ")", right.writer)
      ), canStartStatement = left.canStartStatement || lbracket)
  }


  /** Creates a text statement. */
  private def textStmt(text : String) : Statement =
    new Statement(text + ";")


  /** Creates a statement with a label. */
  private def labeledStmt(text : String, lbl : AnyRef) : Statement =
    new Statement(outClass.seq(
      text, " ", labelWriter(lbl), ";"
    ))


  /** Statement with an expression as argument. */
  private def kwdStmt(kwd :String, expr : Expression) : Statement =
    new Statement(outClass.seq(
      kwd, " ", expr.writer, ";"
    ))


  /** Creates a subcontext writer. */
  private def subContext(locals : Set[AnyRef], writer : OutFragment) : OutFragment =
    ctx ⇒ writer(ctx.sub(locals, Seq.empty))


  /** Creates a writer in a sub-function context. */
  private def subFunction(
        locals : Iterable[AnyRef],
        labels : Iterable[AnyRef],
        writer : OutFragment)
      : OutFragment =
    ctx ⇒ writer(ctx.sub(locals, labels))

  /** Writes item with separator. */
  private def sepBy(separator : OutFragment, items : Iterable[OutFragment]) : OutFragment =
    outClass.seq(intersperse(separator, items) :_*)

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


  /**
   * Creates a new writer for the variable.
   * @param name variable name (identifier).
   */
  private def varWriter(name : AnyRef) : OutFragment =
    ctx ⇒ ctx.writeVariable(name)


  /** Creates a label writer. */
  private def labelWriter(name : AnyRef) : OutFragment =
    ctx ⇒ ctx.writeLabel(name)


  /** Creates a possibly-bracketed expression. */
  private[model] def bracketed(cond : Boolean,
        lbracket : OutFragment, rbracket : OutFragment,
        content : OutFragment) : OutFragment =
    if (cond)
      outClass.seq(lbracket, content, rbracket)
    else
      content


  /** Writes an expression as comma-safe. */
  private def commaSafe(expr : Expression) : OutFragment =
    bracketed(!expr.isCommaSafe, "(", ")", expr.writer)


  /** Creates a member accessor writer. */
  private def memberAccessorWriter(item : Expression) : OutFragment =
    item.simpleMemberAccessor match {
      case None ⇒ outClass.seq("[", item.writer, "]")
      case Some(x) ⇒ outClass.seq(".", x)
    }


  /** Implicit conversion from string to fragment. */
  implicit private def str2fragment(str : String) : OutFragment =
    outClass.token(str)


  /** Checks, if value is valid identifier. */
  private def validSimpleId(value :String) : Boolean = {
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


  def mkFunctionBody(
      args : Seq[AnyRef],
      vars : Seq[AnyRef],
      funcs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      stmt : Seq[Statement]) : FunctionBody = {

    val allLocals : Set[AnyRef] = (
      args ++ vars ++ funcs.map(x ⇒ x._1)).toSet

    val res = new ArrayBuffer[OutFragment]
    res += "(" += sepBy(",", args.map(varWriter)) += "){"

    if (!vars.isEmpty)
      res += "var " += sepBy(",", vars.map(varWriter)) += ";"

    funcs.foreach(f ⇒
      res += outClass.seq("function ", varWriter(f._1), f._2.writer))

    res ++= stmt.map(_.stmtWriter) += "}"

    new FunctionBody(subFunction(allLocals, labels, outClass.seq(res : _*)))
  }
}
