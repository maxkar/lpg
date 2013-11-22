package ru.maxkar.backend.js.model

import java.math.BigInteger
import java.math.BigDecimal

import ru.maxkar.backend.js.out.CompactContext

/** Model element factories.
 * Lacks support of regular expressions at this point.
 */
final object Model {


  /** Failure expression. */
  val failure : LeftValue =
    new LeftValue {
      val priority = 0
      def writeExpression(ctx : CompactContext) : Unit =
        throw new IllegalStateException("Cannot write failure!")
    }


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
    (id, new FunctionBody(args, locals, localFuncs, labels, body))


  /** Creates a string expression. */
  def literal(expr : String) : Expression = {
    new StringExpression(expr)
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
    new Expression {
      val priority = 0
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.write('[')
        ctx.sepby[Expression](elts, ',', _.writeExpressionCommaSafe(ctx))
        ctx.write(']')
      }
  }


  /** Creates an object literal. */
  def objectliteral(elts : (String, Expression)*) : Expression =
    new ObjectExpression(elts)


  /** Creates a reference to a variable in outer scope. */
  def variable(ref : AnyRef) : LeftValue =
    new LeftValue {
      val priority = 0
      def writeExpression(ctx : CompactContext) : Unit =
        ctx.writeVariable(ref)
    }


  /** Creates an anonymous local function. */
  def anonfun(args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression = {

    val fb = new FunctionBody(args, locals, localFuncs, labels, body)
    new Expression {
      val priority = 0
      override val canStartStatement = false
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.write("function")
        fb.writeTo(ctx)
      }
    }
  }


  /** Creates a named (self-referentiable) ocal function. */
  def namedfun(id : AnyRef, args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression = {

    val fb = new FunctionBody(args, locals, localFuncs, labels, body)
    new Expression {
      val priority = 0
      override val canStartStatement = false
      def writeExpression(baseCtx : CompactContext) : Unit = {
        val ctx = baseCtx.sub(Set(id), labels)
        ctx.write("function ")
        ctx.writeVariable(id)
        fb.writeTo(ctx)
      }
    }
  }


  /** Member access expression. */
  def member(base : Expression, item : Expression)
      : LeftValue =
    new LeftValue {
      val priority : Int = 0
      val basebrackets = base.priority > priority
      override val canStartStatement : Boolean = base.canStartStatement || basebrackets
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.bracketed(basebrackets, '(', ')', base.writeExpression(ctx))
        item.writeAsMemberAccessor(ctx)
      }
    }


  /** Creates a new instance creation expression. */
  def create(base : NonprimitiveExpression, args : Expression*)
      : NonprimitiveExpression =
    new NonprimitiveExpression {
      val priority : Int = 1
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.write("new ")
        ctx.bracketed(base.priority > priority, '(', ')',
          base.writeExpression(ctx))

        ctx.write('(')
        ctx.sepby[Expression](args, ',', _.writeExpressionCommaSafe(ctx))
        ctx.write(')')
      }
    }


  /** Writes a function call. */
  def call(base : NonprimitiveExpression, args : Expression*)
      : NonprimitiveExpression =
    new NonprimitiveExpression {
      val priority = 2
      val fnbrackets = base.priority > priority
      override val canStartStatement = base.canStartStatement || fnbrackets
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.bracketed(fnbrackets, '(', ')', base.writeExpression(ctx))
        ctx.write('(')
        ctx.sepby[Expression](args, ',', _.writeExpressionCommaSafe(ctx))
        ctx.write(')')
      }
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
  def neg(base : Expression) : Expression =
    new Expression {
      val priority = 4
      override val isMinusSafe = false
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.write('-')
        ctx.bracketed(base.priority > priority, '(', ')',
          base.writeExpressionAfterMinus(ctx))
      }
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
  def sub(left : Expression, right : Expression) : Expression =
    new Expression {
      val priority = 6
      val lbracket = left.priority > priority
      override val canStartStatement = left.canStartStatement || lbracket
      def writeExpression(ctx : CompactContext) : Unit = {
        val rbracket = right.priority >= priority
        ctx.bracketed(lbracket, '(', ')', left.writeExpression(ctx))
        ctx.write('-')
        ctx.bracketed(rbracket, '(', ')', right.writeExpressionAfterMinus(ctx))
      }
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
      onTrue : Expression, onFalse: Expression) : Expression =
    new Expression {
      val priority = 15
      val lbracket = cond.priority >= priority
      override val canStartStatement = cond.canStartStatement || lbracket
      def writeExpression(ctx : CompactContext) : Unit = {
        val rbracket = onFalse.priority > priority
        ctx.bracketed(lbracket, '(', ')', cond.writeExpression(ctx))
        ctx.write('?')
        onTrue.writeExpression(ctx)
        ctx.write(':')
        ctx.bracketed(rbracket, '(', ')', onFalse.writeExpression(ctx))
      }
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
    new Expression {
      val priority = 18
      override val canStartStatement = first.canStartStatement
      override val isCommaSafe = false
      def writeExpression(ctx : CompactContext) : Unit = {
        first.writeExpression(ctx)
        ctx.write(',')
        second.writeExpression(ctx)
      }
    }




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
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("do{")
        items.foreach(_.writeStatement(ctx))
        ctx.write("}while(")
        cond.writeExpression(ctx)
        ctx.write(");")
      }
    }


  /** Creates a for statement. */
  def whileWithIterupdate(cond : Expression, update : Expression,
      body : Seq[Statement]) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("for(;")
        cond.writeExpression(ctx)
        ctx.write(';')
        update.writeExpression(ctx)
        ctx.write(')')
        ctx.bracketed(body.size != 1, '{', '}',
          body.foreach(_.writeStatement(ctx)))
      }
    }


  /** Performs iteration over container keys. */
  def forIn(iter : LeftValue, collection : Expression,
      body : Seq[Statement]) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("for (")
        iter.writeExpression(ctx)
        ctx.write(" in ")
        collection.writeExpression(ctx)
        ctx.write(')')
        ctx.bracketed(body.size != 1, '{', '}',
          body.foreach(_.writeStatement(ctx)))
      }
    }


  /** Performs statements when condition is true. */
  def when(cond : Expression, body : Seq[Statement]) : Statement =
    doCond(cond, body, Seq.empty)


  /** Chooses one of two statements. */
  def doCond(condition : Expression, onTrue : Seq[Statement],
      onFalse : Seq[Statement]): Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("if(")
        condition.writeExpression(ctx)
        ctx.write(')')
        ctx.bracketed(onTrue.size != 1, '{', '}',
          onTrue.foreach(_.writeStatement(ctx)))
        ctx.write("else")
        if (onFalse.size == 1)
          ctx.write(' ')
        ctx.bracketed(onFalse.size != 1, '{', '}',
          onFalse.foreach(_.writeStatement(ctx)))
      }
    }


  /** Labels a statement. */
  def label(lbl : AnyRef, body : Statement) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.writeLabel(lbl)
        ctx.write(':')
        body.writeStatement(ctx)
      }
    }


  /** Return statement. */
  def returns(value : Expression) : Statement =
    kwdStmt("return", value)


  /** Return-nothing statement. */
  def returnNothing() : Statement =
    textStmt("return")


  /** Switch statement. */
  def switchof(cond : Expression,
      rmap : Seq[(Seq[Expression], Seq[Statement])],
      onElse : Option[Seq[Expression]]) : Statement =
    new SwitchStatement(cond, rmap, onElse)


  /** Throw statement. */
  def throws(value : Expression) : Statement =
    kwdStmt("throw", value)


  /** Try/catch statement. */
  def tryCatch( body : Seq[Statement],
      exnId : AnyRef,
      exnHandler : Seq[Statement]) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("try{")
        body.foreach(_.writeStatement(ctx))
        ctx.write("}catch(")
        val ss = ctx.sub(Set(exnId), Seq.empty)
        ss.writeVariable(exnId)
        ss.write("){")
        exnHandler.foreach(_.writeStatement(ss))
        ss.write("}")
      }
    }


  /** Tries with finalizer. */
  def withFin(body : Seq[Statement], fin : Seq[Statement]) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("try{")
        body.foreach(_.writeStatement(ctx))
        ctx.write("}finally{")
        fin.foreach(_.writeStatement(ctx))
        ctx.write("}")
      }
    }


  /** While statement. */
  def whiles(cond : Expression, body : Seq[Statement]) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write("while(")
        cond.writeExpression(ctx)
        ctx.write(')')
        ctx.bracketed(body.size != 1, '{', '}',
          body.foreach(_.writeStatement(ctx)))
      }
    }




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

    new JSFile(globals.toMap, vars, funcs, inits)
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
    new Expression {
      val priority = 0
      override val isMinusSafe = !value.startsWith("-")
      def writeExpression(ctx : CompactContext) : Unit =
        ctx.write(value)
    }


  /** Creates a prefix operation. */
  private def prefixOp(peer : LeftValue, op : String) : Expression =
    new Expression {
      val priority = 2
      override val isMinusSafe = !op.startsWith("-")
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.write(op)
        peer.writeExpression(ctx)
      }
    }


  /** Creates a postfix operation. */
  private def postfixOp(peer : LeftValue, op : String) : Expression =
    new Expression {
      val priority = 2
      override val canStartStatement = peer.canStartStatement
      def writeExpression(ctx : CompactContext) : Unit = {
        peer.writeExpression(ctx)
        ctx.write(op)
      }
    }


  /** Creates a new unary expression. */
  private def unary(base : Expression, code : String) : Expression =
    new Expression {
      val priority = 4
      def writeExpression(ctx : CompactContext) : Unit = {
        ctx.write(code)
        ctx.bracketed(base.priority > priority, '(', ')',
          base.writeExpression(ctx))
      }
    }


  /** Creates a binary expression. */
  private def binary(
      left : Expression, right : Expression, sign : String,
      bpriority : Int) : Expression =
    new Expression {
      val priority = bpriority
      val lbracket = left.priority > priority
      override val canStartStatement = left.canStartStatement || lbracket
      def writeExpression(ctx : CompactContext) : Unit = {
        val rbracket = right.priority >= priority
        ctx.bracketed(lbracket, '(', ')', left.writeExpression(ctx))
        ctx.write(sign)
        ctx.bracketed(rbracket, '(', ')', right.writeExpression(ctx))
      }
    }


  /** Creates a text statement. */
  private def textStmt(text : String) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write(text)
        ctx.write(';')
      }
    }


  /** Creates a statement with a label. */
  private def labeledStmt(text : String, lbl : AnyRef) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write(text)
        ctx.write(' ')
        ctx.writeLabel(lbl)
        ctx.write(';')
      }
    }


  /** Statement with an expression as argument. */
  def kwdStmt(kwd :String, expr : Expression) : Statement =
    new Statement {
      def writeStatement(ctx : CompactContext) : Unit = {
        ctx.write(kwd)
        ctx.write(' ')
        expr.writeExpression(ctx)
        ctx.write(';')
      }
    }

}
