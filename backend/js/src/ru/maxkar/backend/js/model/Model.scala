package ru.maxkar.backend.js.model

import java.math.BigInteger
import java.math.BigDecimal

/** Model element factories.
 * Lacks support of regular expressions at this point.
 */
final object Model {
  /** Boolean "true" expression. */
  val exprTrue : Expression = new TextExpression("true")
  /** Boolean "false" expression. */
  val exprFalse : Expression = new TextExpression("false")
  /** Null expression. */
  val exprNull : Expression = new TextExpression("null")
  /** Undefined expression. */
  val undefined : Expression = new TextExpression("undefined")
  /** Function arguments expression. */
  val arguments : Expression = new TextExpression("arguments")

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
    numLit(value.toString)

  /** Creates a numeric literal expression. */
  def literal(value : Long) : Expression =
    numLit(value.toString)

  /** Creates a numeric literal experssion. */
  def literal(value : BigInteger) : Expression =
    numLit(value.toString)

  /** Creates a number literal expression. */
  def literal(value : Double) : Expression =
    numLit(value.toString)

  /** Creates a number literal expression. */
  def literal(head : BigDecimal, tail : BigInteger) : Expression =
    numLit(head.toString + "E" + tail.toString)

  /** Creates a boolean literal. */
  def literal(value : Boolean) : Expression =
    if (value) exprTrue else exprFalse

  /** Creates an array expression. */
  def arrayliteral(elts : Expression*) : Expression =
    new ArrayExpression(elts)

  /** Creates an object literal. */
  def objectliteral(elts : (String, Expression)*) : Expression =
    new ObjectExpression(elts)

  /** Creates a global variable/reference. */
  def global(name : String) : Global =
    new Global(name)

  /** Creates a reference to a variable in outer scope. */
  def variable(ref : AnyRef) : Variable =
    new Variable(ref)

  /** Creates an anonymous local function. */
  def anonfun(args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression =
    new AnonfunExpression(new FunctionBody(args, locals, localFuncs, labels, body))

  /** Creates a named (self-referentiable) ocal function. */
  def namedfun(id : AnyRef, args : Seq[AnyRef], locals : Seq[AnyRef],
      localFuncs : Seq[(AnyRef, FunctionBody)],
      labels : Seq[AnyRef],
      body : Seq[Statement]) : Expression =
    new NamedfunExpression(id, new FunctionBody(args, locals, localFuncs, labels, body))

  /** Member access expression. */
  def member(base : Expression, item : Expression)
    : NonprimitiveExpression =
      new MemberAccessExpression(base, item)

  /** Creates a new instance. */
  def create(base : NonprimitiveExpression, args : Expression*)
    : NonprimitiveExpression =
      new NewExpression(base, args)


  /** Writes a function call. */
  def call(base : NonprimitiveExpression, args : Expression*)
    : NonprimitiveExpression =
      new CallExpression(base, args)

  /** Prefix increment expression. */
  def prefixInc(base : LeftValue) : Expression =
    new PrefixOp(base, "++")

  /** Prefix decrement expression. */
  def prefixDec(base : LeftValue) : Expression =
    new PrefixDecrement(base)

  /** Prefix increment expression. */
  def postfixInc(base : LeftValue) : Expression =
    new PostfixOp(base, "++")

  /** Prefix decrement expression. */
  def postfixDec(base : LeftValue) : Expression =
    new PostfixOp(base, "--")

  /** Logical not. */
  def boolNot(base : Expression) : Expression =
    new UnaryExpression(base, "!")

  /** Bitwise not. */
  def bitNot(base : Expression) : Expression =
    new UnaryExpression(base, "~")

  /** Negation expression. */
  def neg(base : Expression) : Expression =
    new UnaryMinusExpression(base)

  /** "Type of" expression. */
  def typeof(base : Expression) : Expression =
    new UnaryExpression(base, "typeof ")

  /** "Void" expression. */
  def voidof(base : Expression) : Expression =
    new UnaryExpression(base, "void ")

  /** "Delete" expression. */
  def delete(base : NonprimitiveExpression) : Expression =
    new UnaryExpression(base, "delete ")

  /** Multiplication expression. */
  def mul(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "*", 5)

  /** Division expression. */
  def div(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "/", 5)

  /** Remainder expression. */
  def rem(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "%", 5)

  /** Addition expression. */
  def add(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "+", 6)

  /** Subtraction expression. */
  def sub(left : Expression, right : Expression) : Expression =
    new BinaryMinusExpression(left, right)

  /** Shift-left expression. */
  def shl(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "<<", 7)

  /** Signed shift right expression. */
  def sshr(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, ">>", 7)

  /** Unsighed shift right expression. */
  def ushr(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, ">>>", 7)

  /** Less comparison expression. */
  def less(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "<", 8)

  /** Less or equals comparison expression. */
  def lessEq(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "<=", 8)

  /** Greater comparison expression. */
  def greater(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, ">", 8)

  /** Greator or equals expression. */
  def greaterEq(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, ">=", 8)

  /** "In" check expression. */
  def isIn(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, " in ", 8)

  /** Conditional cast expression. */
  def testInstanceOf(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, " instanceof ", 8)

  /** Equals expression. */
  def equalsTo(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "==", 9)

  /** Not equals expression. */
  def notEquals(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "!=", 9)

  /** Strict equality comparison. */
  def strictEquals(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "===", 9)

  /** String non-equals expression. */
  def strictNotEquals(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "!==", 9)

  /** Bitwise and expression. */
  def bitAnd(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "&", 10)

  /** Bitwise exclusive or expression. */
  def bitXor(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "^", 11)

  /** Bitwise or expression. */
  def bitOr(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "|", 12)

  /** Boolean and expression. */
  def boolAnd(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "&&", 13)

  /** Boolean or expression. */
  def boolOr(left : Expression, right : Expression) : Expression =
    new BinaryExpression(left, right, "||", 14)

  /** Conditional expression. */
  def cond(condition : Expression,
      onTrue : Expression, onFalse: Expression) : Expression =
    new TernaryExpression(condition, onTrue, onFalse)

  /** Assignment expression. */
  def assign(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "=")

  /** Inplace addition expression. */
  def inplaceAdd(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "+=")

  /** Inplace subtraction expression. */
  def inplaceSub(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "-=")

  /** Inplace multiplication expression. */
  def inplaceMul(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "*=")

  /** Inplace division expression. */
  def inplaceDiv(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "/=")

  /** Inplace remainder expression. */
  def inplaceRem(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "%=")

  /** Inplace shilt left expression. */
  def inplaceShl(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "<<=")

  /** Inplace signed shift right expression. */
  def inplaceSshr(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, ">>=")

  /** Inplace unsigned shift right expression. */
  def inplaceUshr(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, ">>>=")

  /** Inplace bitwise and expression. */
  def inplaceBitAnd(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "&=")

  /** Inplace bitwise or expression. */
  def inplaceBitOr(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "|=")

  /** Inplace bitwise exclusive or expression. */
  def inplaceBitXor(host : LeftValue, value : Expression) : Expression =
    new AssignmentExpression(host, value, "^=")

  /** Sequence (comma) expression. */
  def seqExpr(first : Expression, second : Expression) : Expression =
    new CommaExpression(first, second)



  /** Breaks from an outer statement. */
  def breakOuter() : Statement = new TextStatement("break")

  /** Breaks a labeled statement. */
  def breakL(label : AnyRef) = new LabeledStatement("break", label)

  /** Continues an outer statement. */
  def continueOuter() : Statement = new TextStatement("continue")

  /** Continues a labeled statement. */
  def continueL(label : AnyRef) = new LabeledStatement("continue", label)

  /** Preforms statement with a postfix condition check. */
  def doWhile(statements : Seq[Statement], cond : Expression) : Statement =
    new DoWhileStatement(statements, cond)

  /** Creates a for statement. */
  def whileWithIterupdate(cond : Expression, update : Expression,
      body : Seq[Statement]) : Statement =
    new ForStatement(cond, update, body)

  /** Performs iteration over container keys. */
  def forIn(iter : LeftValue, collection : Expression,
      body : Seq[Statement]) : Statement =
    new ForInStatement(iter, collection, body)

  /** Performs statements when condition is true. */
  def when(cond : Expression, body : Seq[Statement]) : Statement =
    new WhenStatement(cond, body)

  /** Chooses one of two statements. */
  def doCond(condition : Expression, onTrue : Seq[Statement],
      onFalse : Seq[Statement]): Statement =
    new IfStatement(condition, onTrue, onFalse)

  /** Labels a statement. */
  def label(lbl : AnyRef, body : Statement) : Statement =
    new LabelStatement(lbl, body)

  /** Return statement. */
  def returns(value : Expression) : Statement =
    new KwdExpressionStatement("return", value)

  /** Switch statement. */
  def switchof(cond : Expression,
      rmap : Seq[(Seq[Expression], Seq[Statement])],
      onElse : Option[Seq[Expression]]) : Statement =
    new SwitchStatement(cond, rmap, onElse)

  /** Throw statement. */
  def throws(value : Expression) : Statement =
    new KwdExpressionStatement("throw", value)

  /** Try/catch statement. */
  def tryCatch( body : Seq[Statement],
      exnId : AnyRef,
      exnHandler : Seq[Statement]) : Statement =
    new TryCatchStatement(body, exnId, exnHandler)

  /** Tries with finalizer. */
  def withFin(body : Seq[Statement], fin : Seq[Statement]) : Statement =
    new TryFinallyStatement(body, fin)

  /** While statement. */
  def whiles(cond : Expression, body : Seq[Statement]) : Statement =
    new WhileStatement(cond, body)




  /** Creates a javascript file. */
  def file(
      extGlobals : Iterable[String],
      vars : Seq[String],
      funcs :Seq[(String, FunctionBody)],
      inits :Seq[Statement]) : JSFile =
    new JSFile(extGlobals, vars, funcs, inits)



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


  /** Constructs a numeric literal. */
  private def numLit(value : String) : Expression =
    if (value.startsWith("-"))
      new NegativeNumberExpression(value)
    else
      new TextExpression(value)

}
