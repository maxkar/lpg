package ru.maxkar.backend.js

import ru.maxkar.backend.js.model._
import ru.maxkar.backend.js.model.Model._

import org.junit._
import org.junit.Assert._

import org.scalatest.junit.JUnitSuite

/** Tests for js compact writing. */
final class TestCompactWrite extends JUnitSuite {
  import TestCompactWrite._


  /** Tests a primitive expressions. */
  @Test
  def testPrimitives() : Unit = {
    "true;" ?= exprTrue
    "false;" ?= exprFalse
    "null;" ?= exprNull
    "undefined;" ?= undefined
    "arguments;" ?= arguments
  }

  /** Tests string literal. */
  @Test
  def testStringLiteral() : Unit = {
    "\"abc\";" ?= literal("abc")
    "\"abc\\ndef\";" ?= literal("abc\ndef")
    "\"a\\\"b\";" ?= literal("a\"b")
  }


  /** Tests numeric literals. */
  @Test
  def testNumLiteral() : Unit = {
    "123;" ?= literal(123)
    "-123;" ?= literal(-123)
    "- -123;" ?= neg(-123)
  }


  /** Tests array literal. */
  @Test
  def testArrayLiteral() : Unit = {
    "[1,2,3];" ?= arrayliteral(1,2,3)
    "[(1,2),(3,4)];" ?= arrayliteral(seqExpr(1,2), seqExpr(3,4))
    "[[[]]];" ?= arrayliteral(arrayliteral(arrayliteral()))
  }

  /** Tests object literal. */
  @Test
  def testObjectLiteral() : Unit = {
    "({x:3,y:4});" ?= objectliteral(("x", 3), ("y", 4))
    "({\"\":3,y:4});" ?= objectliteral(("", 3), ("y", 4))
    "({\" \":3});" ?= objectliteral((" ", 3))
    "({});" ?= objectliteral()
    "({x:(3,4),y:5});" ?= objectliteral(("x", seqExpr(3,4)), ("y", 5))
  }

  /** Tests local variables. */
  @Test
  def testLocalRefs1() : Unit = {
    val df = defun("test", Seq("x1", "x2"), Seq.empty, Seq.empty, Seq.empty,
      Seq(variable("x1"), variable("x2")))
    val f = file(Seq(("test","test")), Seq.empty, Seq(("test", df._2)), Seq.empty)

    "function test(a,b){a;b;}" ?= f
  }

  /** Tests anonymous function. */
  @Test
  def testAnonfun() : Unit = {
    "1+function(a,b){a;b;};" ?=
      add(1, anonfun(Seq("x1", "x2"), Seq.empty, Seq.empty, Seq.empty,
        Seq(variable("x1"), variable("x2"))))
  }

  /** Tests nonanonymous function. */
  @Test
  def testNamedfun() : Unit = {
    "1+function a(b,c){a;};" ?=
      add(1, namedfun("z", Seq("x1", "x2"), Seq.empty, Seq.empty, Seq.empty,
        Seq(variable("z"))))
  }

  /** Tests function escape at a statement level. */
  @Test
  def testFunEsc() : Unit = {
    "(function a(b,c){a;}+1);" ?=
      add(namedfun("z", Seq("x1", "x2"), Seq.empty, Seq.empty, Seq.empty,
        Seq(variable("z"))), 1)
    "(function(a,b){a;b;}+1);" ?=
      add(anonfun(Seq("x1", "x2"), Seq.empty, Seq.empty, Seq.empty,
        Seq(variable("x1"), variable("x2"))), 1)
  }


  /** Tests a member access. */
  @Test
  def testMember() : Unit = {
    "g1.y;" ?= member(variable("g1"), "y")
    "g1[\"\"];" ?= member(variable("g1"), "")
    "g1[23];" ?= member(variable("g1"), 23)
    "g1[\"this is a test\"];" ?= member(variable("g1"), "this is a test")
  }


  /** Tests a creation expression. */
  @Test
  def testCreation() : Unit = {
    "new g1();" ?= create(variable("g1"))
    "new g1(1,2,3);" ?= create(variable("g1"), 1, 2, 3)
    "new g1.x(1,2,3);" ?= create(member(variable("g1"), "x"), 1, 2, 3)
    "new g1[2](1,2,3);" ?= create(member(variable("g1"), 2), 1, 2, 3)
  }


  /** Tests a call expression. */
  @Test
  def testCall() : Unit = {
    "g1(ga,3,4);" ?= call(variable("g1"), variable("ga"), 3, 4)
    "g1()();" ?= call(call(variable("g1")))
  }


  /** Tests a prefix/postfix modifiers. */
  @Test
  def testPrefixPostfixMods() : Unit = {
    "++g1;" ?= prefixInc(variable("g1"))
    "--g1;" ?= prefixDec(variable("g1"))
    "g1++;" ?= postfixInc(variable("g1"))
    "g1--;" ?= postfixDec(variable("g1"))
    "- --g1;" ?= neg(prefixDec(variable("g1")))
  }


  /** Tests for tier-4 expressions. */
  @Test
  def testTier4() : Unit = {
    "!!g1;" ?= boolNot(boolNot(variable("g1")))
    "~g1;" ?= bitNot(variable("g1"))
    "-g1;" ?= neg(variable("g1"))
    "- -g1;" ?= neg(neg(variable("g1")))
    "- - -3;" ?= neg(neg(-3))
    "typeof 3;" ?= typeof(3)
    "void typeof 3;" ?= voidof(typeof(3))
    "delete g1;" ?= delete(variable("g1"))
    "-(1+2);" ?= neg(add(1,2))
  }

  /** Tests for tier-5 expressions. */
  @Test
  def testTier5() : Unit = {
    "3*4;" ?= mul(3,4)
    "3/4;" ?= div(3,4)
    "3%5;" ?= rem(3,5)
    "3%- -2;" ?= rem(3, neg(-2))
  }

  /** Tests tier-6 expressions. */
  @Test
  def testTier6() : Unit = {
    "1+2;" ?= add(1,2)
    "1-2;" ?= sub(1,2)
    "1+2*3;" ?= add(1, mul(2,3))
    "2*3+1;" ?= add(mul(2,3), 1)
    "(1+2)*3;" ?= mul(add(1,2), 3)
  }

  /** Tests tier-7 expressions. */
  @Test
  def testTier7() : Unit = {
    "1<<2;" ?= shl(1,2)
    "14>>1;" ?= sshr(14, 1)
    "14>>>1;" ?= ushr(14, 1)
  }

  /** Tests tier-8 expressions. */
  @Test
  def testTier8() : Unit = {
    "1<2;" ?= less(1,2)
    "1>2;" ?= greater(1,2)
    "1<=2;" ?= lessEq(1,2)
    "1>=2;" ?= greaterEq(1,2)
    "1 in [1,2];" ?= isIn(1, arrayliteral(1,2))
    "1 instanceof 2;" ?= testInstanceOf(1, 2)
  }

  /** Tests tier-9 expressions. */
  @Test
  def testTier9() : Unit = {
    "1==2;" ?= equalsTo(1,2)
    "1!=2;" ?= notEquals(1,2)
    "1===2;" ?= strictEquals(1,2)
    "1!==2;" ?= strictNotEquals(1,2)
  }

  /** Tests tiers 11 to 15. */
  @Test
  def testTier11_15() : Unit = {
    "1&2;" ?= bitAnd(1,2)
    "1^2;" ?= bitXor(1,2)
    "1|2;" ?= bitOr(1,2)
    "1&&2;" ?= boolAnd(1,2)
    "1||2;" ?= boolOr(1,2)
  }

  /** Tests assignments. */
  @Test
  def testAssigns() : Unit = {
    val g = variable("g1")
    "g1=2;" ?= assign(g, 2)
    "g1+=2;" ?= inplaceAdd(g, 2)
    "g1-=2;" ?= inplaceSub(g, 2)
    "g1*=2;" ?= inplaceMul(g, 2)
    "g1/=2;" ?= inplaceDiv(g, 2)
    "g1%=2;" ?= inplaceRem(g, 2)
    "g1<<=2;" ?= inplaceShl(g, 2)
    "g1>>=2;" ?= inplaceSshr(g, 2)
    "g1>>>=2;" ?= inplaceUshr(g, 2)
    "g1&=1;" ?= inplaceBitAnd(g, 1)
    "g1|=1;" ?= inplaceBitOr(g, 1)
    "g1^=1;" ?= inplaceBitXor(g, 1)
  }

  /** Tests a comma expression. */
  @Test
  def testComma() : Unit = {
    "1,2,3;" ?= seqExpr(seqExpr(1,2),3)
    "g1((1,2),3);" ?= call(variable("g1"), seqExpr(1,2), 3)
    "[(1,2)];" ?= arrayliteral(seqExpr(1,2))
    "({a:(1,2)});" ?= objectliteral(("a", seqExpr(1,2)))
  }


  /** Tests a break statements. */
  @Test
  def testBreaks() : Unit = {
    "break;" ?= breakOuter

    "(function(){a:break a;});" ?=
      anonfun(Seq.empty, Seq.empty, Seq.empty, Seq("x1"),
        Seq(label("x1", breakL("x1"))))
  }

  /** Tests continue statements. */
  @Test
  def testContinues() : Unit = {
    "continue;" ?= continueOuter

    "(function(){a:continue a;});" ?=
      anonfun(Seq.empty, Seq.empty, Seq.empty, Seq("x1"),
        Seq(label("x1", continueL("x1"))))
  }


  /** Tests a do-while expression. */
  @Test
  def testDoWhile() : Unit = {
    "do{1;2;}while(3);" ?= doWhile(Seq(1,2), 3)
    "do{1;}while(3);" ?= doWhile(Seq(1), 3)
    "do{}while(3);" ?= doWhile(Seq(), 3)
  }

  /** Tests a classical for loop. */
  @Test
  def testClassicalFor() : Unit = {
    "for(;1;2){3;4;}" ?= whileWithIterupdate(1,2, Seq(3,4))
    "for(;1;2)3;" ?= whileWithIterupdate(1,2, Seq(3))
    "for(;1;2){}" ?= whileWithIterupdate(1,2, Seq())
    "for(;1,2;3,4){}" ?= whileWithIterupdate(seqExpr(1,2), seqExpr(3,4), Seq())
  }

  /** Tests a for-in statement. */
  @Test
  def testForIn() : Unit = {
    "for (g1 in []){2;3;}" ?= forIn(variable("g1"), arrayliteral(), Seq(2, 3))
    "for (g1 in [])2;" ?= forIn(variable("g1"), arrayliteral(), Seq(2))
    "for (g1 in []){}" ?= forIn(variable("g1"), arrayliteral(), Seq())
    "for (g1 in 1,2){}" ?= forIn(variable("g1"), seqExpr(1,2), Seq())
  }


  /** Tests an if statement. */
  @Test
  def testIf() : Unit = {
    "if(1){2;3;}else{}" ?= when(1, Seq(2,3))
    "if(1)2;else{}" ?= when(1, Seq(2))
    "if(1){}else{}" ?= when(1, Seq())
    "if(1){2;3;}else{4;5;}" ?= doCond(1, Seq(2,3), Seq(4,5))
    "if(1)3;else{4;5;}" ?= doCond(1, Seq(3), Seq(4,5))
    "if(1){2;3;}else 5;" ?= doCond(1, Seq(2,3), Seq(5))
    "if(1)2;else 5;" ?= doCond(1, Seq(2), Seq(5))
    "if(1){}else{4;5;}" ?= doCond(1, Seq(), Seq(4,5))
    "if(1){2;3;}else{}" ?= doCond(1, Seq(2,3), Seq())
    "if(1)2;else{}" ?= doCond(1, Seq(2), Seq())
    "if(1){}else 4;" ?= doCond(1, Seq(), Seq(4))
    "if(1){}else{}" ?= doCond(1, Seq(), Seq())
  }

  /** Tests a return statement. */
  @Test
  def testReturns() : Unit = {
    "return 1;" ?= returns(1)
  }

  /** Tests a switch statement. */
  @Test
  def testSwitch() : Unit = {
    "switch(1){case 2:case 3:4;5;break;case 6:break;}" ?=
      switchof(1, Seq((Seq(2,3), Seq(4,5)), (Seq(6), Seq())), None)
    "switch(1){case 2:break;default:4;3;}" ?=
      switchof(1, Seq((Seq(2), Seq())), Some(Seq(4,3)))
  }

  /** Tests a throw statement. */
  @Test
  def testThrow() : Unit = {
    "throw 1;" ?= throws(1)
  }

  /** Tests a catch exception. */
  @Test
  def testCatch() : Unit = {
    "try{1;2;}catch(a){a;4;}" ?=
      tryCatch(Seq(1,2), "zz", Seq(variable("zz"),4))
    "try{1;2;}catch(a){a;}" ?=
      tryCatch(Seq(1,2), "zz", Seq(variable("zz")))
    "try{1;2;}catch(a){}" ?=
      tryCatch(Seq(1,2), "zz", Seq())
    "try{1;}catch(a){}" ?=
      tryCatch(Seq(1), "zz", Seq())
    "try{}catch(a){}" ?=
      tryCatch(Seq(), "zz", Seq())
  }

  /** Tests try/finally. */
  @Test
  def testTryFinally() : Unit = {
    "try{1;2;}finally{3;4;}" ?= withFin(Seq(1,2), Seq(3,4))
    "try{1;}finally{3;4;}" ?= withFin(Seq(1), Seq(3,4))
    "try{1;2;}finally{3;}" ?= withFin(Seq(1,2), Seq(3))
    "try{}finally{3;4;}" ?= withFin(Seq(), Seq(3,4))
    "try{1;2;}finally{}" ?= withFin(Seq(1,2), Seq())
    "try{}finally{}" ?= withFin(Seq(), Seq())
  }

  /** Tests a while statement. */
  @Test
  def testWhile() : Unit = {
    "while(1){2;3;}" ?= whiles(1, Seq(2,3))
    "while(1)2;" ?= whiles(1, Seq(2))
    "while(1){}" ?= whiles(1, Seq())
    "while(1,2){}" ?= whiles(seqExpr(1,2), Seq())
  }
}


private object TestCompactWrite {
  import scala.language.implicitConversions


  implicit def str2expr(str : String) : Expression =
    literal(str)

  implicit def int2expr(str : Int) : Expression =
    literal(str)


  implicit class Str2Assert(val base : String) extends AnyVal {
    def ?=(other : JSFile) : Unit = {
      val sw = new java.io.StringWriter
      other.writeToWriter(sw)
      Assert.assertEquals(base, sw.toString)
    }

    def ?=(other : Statement) : Unit = {
      val body = file(
        Seq("ga", "gb", "gc", "g1").map(x ⇒ (x,x)),
        Seq.empty, Seq.empty, Seq(other))
      this ?= body
    }
  }
}
