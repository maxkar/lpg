package ru.maxkar.hunk

import org.junit._
import org.junit.Assert._

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.JUnitSuite

import Hunk._

/** Tests for hunks. */
final class TestHunks extends JUnitSuite {
  @Test
  def testDeferredHunk() : Unit = {
    val dh = new DeferredHunk[String]
    var x = 1
    var y = 2
    val res = new HunkSuccess[String]("abc")

    dh.onComplete(t ⇒ { x += 2 })
    assertEquals(None, dh.immediateResult)
    assertEquals(1, x)
    assertEquals(2, y)

    dh.resolve(res)

    assertEquals(Some(res), dh.immediateResult)
    assertEquals(3, x)
    assertEquals(2, y)

    dh.onComplete(t ⇒  { y = 4 } )
    assertEquals(3, x)
    assertEquals(4, y)
  }


  @Test
  def testFmap() : Unit = {
    val r1 = new DeferredHunk[String]
    val r2 = r1.fmap(x ⇒ x + "!")
    val r3 = r2.fmap(x ⇒ x + "11")


    val t1 = HunkSuccess("abc")
    val t2 = HunkSuccess("abc!")
    val t3 = HunkSuccess("abc!11")

    assertEquals(None, r1.immediateResult)
    assertEquals(None, r2.immediateResult)
    assertEquals(None, r3.immediateResult)

    r1.succeed("abc")
    assertEquals(Some(t1), r1.immediateResult)
    assertEquals(Some(t2), r2.immediateResult)
    assertEquals(Some(t3), r3.immediateResult)
  }


  @Test
  def testFmapExn() : Unit = {
    val exn = new IllegalArgumentException("!!!")
    val r1 = new DeferredHunk[String]
    val r2 = r1.fmap(_ ⇒  throw exn )

    assertEquals(None, r1.immediateResult)
    assertEquals(None, r2.immediateResult)
    val t1 = HunkSuccess("abc")
    val t2 = HunkException(exn)

    r1.succeed("abc")

    assertEquals(Some(t1), r1.immediateResult)
    assertEquals(Some(t2), r2.immediateResult)
  }


  @Test
  def testMonadicFunc() : Unit = {
    val r1 = new DeferredHunk[Int]
    val r2 = new DeferredHunk[String]
    def fn(x : Int) : Hunk[String] = r2

    val r3 = fn _ <**> r1


    val t1 = HunkSuccess(1)
    val t2 = HunkSuccess("Yeah!")
    val t3 = HunkSuccess("Yeah!")

    assertEquals(None, r1.immediateResult)
    assertEquals(None, r2.immediateResult)
    assertEquals(None, r3.immediateResult)

    r1.succeed(1)
    assertEquals(Some(t1), r1.immediateResult)
    assertEquals(None, r2.immediateResult)
    assertEquals(None, r3.immediateResult)

    r2.succeed("Yeah!")
    assertEquals(Some(t1), r1.immediateResult)
    assertEquals(Some(t2), r2.immediateResult)
    assertEquals(Some(t3), r3.immediateResult)
  }
}
