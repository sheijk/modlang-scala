package me
package modlang
package tfi_trait

// consider adding more types, like TypedExpr[T], etc.

object Calc_bool:
  trait Lang:
    type Expr
    def bool(v: Boolean): Expr
    def and(lhs: Expr, rhs: Expr): Expr

  class ToString extends Lang:
    type Expr = String

    override def bool(v: Boolean): Expr = v.toString()
    override def and(lhs: Expr, rhs: Expr): Expr = s"($lhs & $rhs)"

  class Eval extends Lang:
    type Expr = Boolean

    override def bool(v: Boolean): Expr = v
    override def and(lhs: Boolean, rhs: Boolean): Expr = lhs & rhs

  def tests() =
    List(
      (true, (l: Lang) => l.bool(true)),
      (true, (l: Lang) => l.and(l.bool(true), l.bool(true))),
      (false, (l: Lang) => l.and(l.bool(true), l.bool(false))),
    )

  def runTest(t: (Boolean, (l: Lang) => l.Expr)) =
    val source = t._2(ToString())
    val result = t._2(Eval())
    val expected = t._1
    println(s"Running $source")
    if expected != result then println(s"error: got $result but expected $expected")

  def testing() = tests().foreach(runTest)

object Calc_int:
  trait Lang:
    type Expr
    def int(v: Int): Expr
    def plus(lhs: Expr, rhs: Expr): Expr

  class ToString extends Lang:
    type Expr = String

    override def int(v: Int): Expr = v.toString()
    override def plus(lhs: Expr, rhs: Expr): Expr = s"($lhs + $rhs)"

  class Eval extends Lang:
    type Expr = Int

    override def int(v: Int): Expr = v
    override def plus(lhs: Int, rhs: Int): Expr = lhs + rhs

  def tests() =
    List(
      (10, (l: Lang) => l.int(10)),
      (20, (l: Lang) => l.plus(l.int(5), l.int(15))),
    )

  def runTest(t: (Int, (l: Lang) => l.Expr)) =
    val source = t._2(ToString())
    val result = t._2(Eval())
    val expected = t._1
    println(s"Running $source")
    if expected != result then println(s"error: got $result but expected $expected")

  def testing() = tests().foreach(runTest)

def demo() =
  println("Running tfi_trait demo")
  Calc_bool.testing()
  Calc_int.testing()
