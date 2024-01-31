package me
package modlang
package tfi

// consider adding more types, like TypedExpr[T], etc.

type Test[Value, Lang[_] <: Empty.Lang[?]] = (Value, [T] => (l: Lang[T]) => l.Expr)

def runProgram[Value, Lang[_] <: Empty.Lang[?]](
  f: [T] => (l: Lang[T]) => l.Expr
)(using
  s: Lang[String],
  e: Lang[Value],
): Unit =
  val source = f(s)
  val result = e.eval(f(e))
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
  val result = e.eval(program(e))
  println(s"Running $source produced $result")
  if result != expected then println(s"ERROR: expected $expected but found $result")

def demo() =
  println("Running tfi_trait demo")
  val simple = [T] => (l: Calc_bool.Lang[T]) => l.and(l.bool(true), l.bool(true))
  val calc: Calc.Program = [T] =>
    (l: Calc.Lang[T]) => l.greaterThan(l.int(10), l.plus(l.int(5), l.int(4)))
  given Calc.ToString()
  runProgram[Boolean, Calc_bool.Lang](simple)(using e = Calc_bool.Eval())
  given Calc.Eval()
  runProgram[Calc.Value, Calc.Lang](simple.asInstanceOf[Calc.Program])
  runProgram[Calc.Value, Calc.Lang](calc)
  println("  Calc_bool")
  Calc_bool.testing()
  println("  Calc_int")
  Calc_int.testing()
  println("  Calc")
  Calc.testing()
  println("  Algo_calc")
  Algo_calc.testing()
  println("  Algo_calc_bindings")
  Algo_calc_bindings.testing()
