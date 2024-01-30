package me
package modlang
package tfi_trait

// consider adding more types, like TypedExpr[T], etc.

object Empty:
  trait Lang[T]:
    type Expr = T

type Test[Value, Lang[_] <: Empty.Lang[?]] = (Value, [T] => (l: Lang[T]) => l.Expr)

def runProgram[Value, Lang[_] <: Empty.Lang[?]](
  f: [T] => (l: Lang[T]) => l.Expr
)(using
  s: Lang[String],
  e: Lang[Value],
): Unit =
  val source = f(s)
  val result = f(e)
  println(s"Running $source produced $result")

def runTest[Value, Lang[_] <: Empty.Lang[?]](
  t: (Value, [T] => (l: Lang[T]) => l.Expr)
)(using
  s: Lang[String],
  e: Lang[Value],
): Unit =
  val expected = t._1
  val program = t._2
  val source = program(s)
  val result = program(e)
  println(s"Running $source produced $result")
  if result != expected then println(s"ERROR: expected $expected but found $result")

object Calc_bool:
  trait Lang[T] extends Empty.Lang[T]:
    def bool(v: Boolean): Expr
    def and(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String]:
    type Expr = String

    def bool(v: Boolean): String = v.toString()
    def and(lhs: String, rhs: String): String = s"($lhs & $rhs)"

  class ToString extends ToStringMixin
  given ToString()

  trait Eval[T] extends Lang[T]:
    override def bool(v: Boolean): Expr = fromBool(v)
    override def and(lhs: Expr, rhs: Expr): Expr = fromBool(asBool(lhs) & asBool(rhs))

    def fromBool(v: Boolean): Expr
    def asBool(t: Expr): Boolean

  type Value = Boolean

  class EvalBool extends Eval[Value]:
    override def fromBool(v: Value): Expr = v
    override def asBool(t: Value): Boolean = t

  given EvalBool()

  def tests() =
    List(
      (true, [T] => (l: Lang[T]) => l.bool(true)),
      (true, [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(true))),
      (false, [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(false))),
    )

  def testing() =
    tests().foreach(runTest[Boolean, Lang])

object Calc_int:
  trait Lang[T] extends Empty.Lang[T]:
    def int(v: Int): Expr
    def plus(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String]:
    type Expr = String

    def int(v: Int): String = v.toString()
    def plus(lhs: String, rhs: String): String = s"($lhs + $rhs)"

  class ToString extends ToStringMixin
  given ToString()

  trait Eval[T] extends Lang[T]:
    override def int(v: Int): Expr = fromInt(v)
    override def plus(lhs: Expr, rhs: Expr): Expr = fromInt(asInt(lhs) + asInt(rhs))

    def fromInt(v: Int): Expr
    def asInt(t: Expr): Int

  type Value = Int

  class EvalInt extends Eval[Value]:
    def fromInt(v: Int) = v
    def asInt(v: Expr) = v

  given EvalInt()

  def tests() =
    List(
      (10, [T] => (l: Lang[T]) => l.int(10)),
      (20, [T] => (l: Lang[T]) => l.plus(l.int(5), l.int(15))),
    )

  def testing() = tests().foreach(runTest[Value, Lang])

object Calc:
  trait Lang[T] extends Calc_int.Lang[T], Calc_bool.Lang[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String], Calc_bool.ToStringMixin, Calc_int.ToStringMixin:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = s"($lhs > $rhs)"

  class ToString extends ToStringMixin
  given ToString()

  trait Eval[T] extends Lang[T], Calc_bool.Eval[T], Calc_int.Eval[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = fromBool(asInt(lhs) > asInt(rhs))

  type Value = Int | Boolean

  class EvalIntBool extends Eval[Value]:
    type Expr = Boolean | Int
    def fromBool(v: Boolean): Expr = v
    def asBool(t: Expr): Boolean = t.asInstanceOf[Boolean]
    def fromInt(v: Int): Expr = v
    def asInt(t: Expr): Int = t.asInstanceOf[Int]

  given EvalIntBool()

  def tests() =
    Calc_int.tests().asInstanceOf[List[MyTest]] ++
      Calc_bool.tests().asInstanceOf[List[MyTest]] ++
      List(
        (
          true,
          [T] =>
            (l: Lang[T]) =>
              l.and(l.greaterThan(l.int(10), l.int(5)), l.greaterThan(l.int(3), l.int(2))),
        )
      )

  type MyTest = Test[Value, Lang]

  def testing() = tests().foreach(runTest[Value, Lang])
  type Program = [T] => (l: Calc.Lang[T]) => l.Expr

def demo() =
  println("Running tfi_trait demo")
  val simple = [T] => (l: Calc_bool.Lang[T]) => l.and(l.bool(true), l.bool(true))
  val calc: Calc.Program = [T] =>
    (l: Calc.Lang[T]) => l.greaterThan(l.int(10), l.plus(l.int(5), l.int(4)))
  given Calc.ToString()
  given Calc_bool.EvalBool()
  given Calc.EvalIntBool()
  runProgram[Boolean, Calc_bool.Lang](simple)
  runProgram[Calc.Value, Calc.Lang](simple.asInstanceOf[Calc.Program])
  runProgram[Calc.Value, Calc.Lang](calc)
  println("  Calc_bool")
  Calc_bool.testing()
  println("  Calc_int")
  Calc_int.testing()
  println("  Calc")
  Calc.testing()
