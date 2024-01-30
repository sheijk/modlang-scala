package me
package modlang
package tfi_trait

// consider adding more types, like TypedExpr[T], etc.

object Empty:
  trait Lang:
    type Expr

type Test[T, Lang <: Empty.Lang] = (T, (l: Lang) => l.Expr)

object Calc_bool:
  trait Lang extends Empty.Lang:
    type Expr
    def bool(v: Boolean): Expr
    def and(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang:
    type Expr = String

    def bool(v: Boolean): String = v.toString()
    def and(lhs: String, rhs: String): String = s"($lhs & $rhs)"

  class ToString extends ToStringMixin

  trait Eval extends Lang:
    type Expr

    override def bool(v: Boolean): Expr = fromBool(v)
    override def and(lhs: Expr, rhs: Expr): Expr = fromBool(asBool(lhs) & asBool(rhs))

    def fromBool(v: Boolean): Expr
    def asBool(t: Expr): Boolean

  class EvalBool extends Eval:
    type Expr = Boolean
    override def fromBool(v: Boolean): Expr = v
    override def asBool(t: Expr): Boolean = t

  def tests() =
    List(
      (true, (l: Lang) => l.bool(true)),
      (true, (l: Lang) => l.and(l.bool(true), l.bool(true))),
      (false, (l: Lang) => l.and(l.bool(true), l.bool(false))),
    )

  type MyTest = Test[Boolean, Lang]
  def runTest(t: MyTest) =
    val source = t._2(ToString())
    val result = t._2(EvalBool())
    val expected = t._1
    println(s"Running $source")
    if expected != result then println(s"error: got $result but expected $expected")

  def testing() = tests().foreach(runTest)

object Calc_int:
  trait Lang extends Empty.Lang:
    type Expr
    def int(v: Int): Expr
    def plus(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang:
    type Expr = String

    def int(v: Int): String = v.toString()
    def plus(lhs: String, rhs: String): String = s"($lhs + $rhs)"

  class ToString extends ToStringMixin

  trait Eval extends Lang:
    override def int(v: Int): Expr = fromInt(v)
    override def plus(lhs: Expr, rhs: Expr): Expr = fromInt(asInt(lhs) + asInt(rhs))

    def fromInt(v: Int): Expr
    def asInt(t: Expr): Int

  class EvalInt extends Eval:
    type Expr = Int
    def fromInt(v: Int) = v
    def asInt(v: Expr) = v

  def tests() =
    List(
      (10, (l: Lang) => l.int(10)),
      (20, (l: Lang) => l.plus(l.int(5), l.int(15))),
    )

  type MyTest = Test[Int, Lang]
  def runTest(t: MyTest) =
    val source = t._2(ToString())
    val result = t._2(EvalInt())
    val expected = t._1
    println(s"Running $source")
    if expected != result then println(s"error: got $result but expected $expected")

  def testing() = tests().foreach(runTest)

object Calc:
  trait Lang extends Calc_int.Lang, Calc_bool.Lang:
    def greaterThan(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang, Calc_bool.ToStringMixin, Calc_int.ToStringMixin:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = s"($lhs > $rhs)"
  class ToString extends ToStringMixin

  trait Eval extends Lang, Calc_bool.Eval, Calc_int.Eval:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = fromBool(asInt(lhs) > asInt(rhs))

  class EvalIntBool extends Eval:
    type Expr = Boolean | Int
    def fromBool(v: Boolean): Expr = v
    def asBool(t: Expr): Boolean = t.asInstanceOf[Boolean]
    def fromInt(v: Int): Expr = v
    def asInt(t: Expr): Int = t.asInstanceOf[Int]

  def tests() =
    Calc_int.tests().asInstanceOf[List[MyTest]] ++
    Calc_bool.tests().asInstanceOf[List[MyTest]] ++
    List(
      (true, (l: Lang) => l.and(l.greaterThan(l.int(10), l.int(5)), l.greaterThan(l.int(3), l.int(2))))
    )

  type MyTest = Test[Int|Boolean, Lang]
  def runTest(t: MyTest) =
    val source = t._2(ToString())
    val result = t._2(EvalIntBool())
    val expected = t._1
    println(s"Running $source")
    if expected != result then println(s"error: got $result but expected $expected")

  def testing() = tests().foreach(runTest)

def demo() =
  println("Running tfi_trait demo")
  println("  Calc_bool")
  Calc_bool.testing()
  println("  Calc_int")
  Calc_int.testing()
  println("  Calc")
  Calc.testing()
