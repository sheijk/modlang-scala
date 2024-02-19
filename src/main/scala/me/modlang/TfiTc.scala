package me
package modlang
package tc

type Test[Value, Lang[_] <: Empty.Lang[?]] = (Value, [T] => (l: Lang[T]) => l.Expr)
type TestLoc[Value, Lang[_] <: Empty.Lang[?]] = (Value, [T] => (l: Lang[T]) => l.Expr, Location)

def runProgram[Value, Lang[_] <: Empty.Lang[?]](
  f: [T] => (l: Lang[T]) => l.Expr
)(using
  s: Lang[String],
  e: Lang[Value],
): Unit =
  val source = f(s)
  val result = e.eval(f(e))
  println(s"  Running $source produced $result")

def runTestLoc[Value, Lang[_] <: Empty.Lang[?]](
  t: (Value, [T] => (l: Lang[T]) => l.Expr, Location)
)(using
  s: Lang[String],
  e: Lang[Value],
): Unit =
  val expected = t._1
  val program = t._2
  val location = t._3
  val source = s.eval(program(s))
  val result = e.eval(program(e))
  println(s"  Running $source produced $result")
  if result != expected then
    println(s"$location: error: expected $expected but found $result")

def demo() =
  println("Running tc demo")
  val simple = [T] => (l: Calc_bool.Lang[T]) => l.and(l.bool(true), l.bool(true))
  val calc: Calc.Program = [T] =>
    (l: Calc.Lang[T]) => l.greaterThan(l.int(10), l.plus(l.int(5), l.int(4)))
  given Calc.ToString()
  runProgram[Boolean, Calc_bool.Lang](simple)(using e = Calc_bool.Eval())
  given Calc.Eval()
  runProgram[Calc.Value, Calc.Lang](simple.asInstanceOf[Calc.Program])
  runProgram[Calc.Value, Calc.Lang](calc)
  imperativeDemo()
  Optimizer.demo()

def imperativeDemo() =
  val program = [T] => (l: Imperative.Lang[T]) =>
      import l.*
      mut("idx", int(0), idx =>
      mut("sum", int(0), sum =>
      loop("l", l =>
        if_(greaterThan(idx.get(), int(10)),
          break(l, sum.get()),
          block(
            idx.set(plus(idx.get(), int(1))),
            sum.set(plus(sum.get(), idx.get()))
            )))))
  given Imperative.ToString()
  given Imperative.Eval()
  runProgram[Imperative.Value, Imperative.Lang](program)
