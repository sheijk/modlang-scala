package me
package modlang
package tc

final class ImperativeSuite extends TestSuite:
  import tfi.*

  def runTestCase[Value, Lang[_] <: Empty.Lang[?]](
    t: (Value, [T] => (l: Lang[T]) => l.Expr, Location)
  )(using
    s: Lang[String],
    e: Lang[Value],
  ): (Value, e.Result, s.Result, Location) =
    val expected = t._1
    val program = t._2
    val source = s.eval(program(s))
    val result = e.eval(program(e))
    (expected, result, source, t._3)

  def runTestCaseOpt[Value, Lang[_] <: Empty.Lang[?]](
    t: ([T] => (l: Lang[T]) => l.Expr, [T] => (l: Lang[T]) => l.Expr, Location),
    evalUnoptimized: Lang[Value],
    toStringUnoptimized: Lang[String],
    eval: Lang[Value],
    toStr: Lang[String],
  ): Unit =
    val expected = t._1
    val program = t._2
    val loc = t._3
    val expectedValue = eval.eval(expected(eval))
    val expectedSource = toStr.eval(expected(toStr))
    val optimizedSource = toStringUnoptimized.eval(expected(toStringUnoptimized))
    val result = eval.eval(program(eval))
    expectEquals(expectedValue, result, "eval optimized code")(using toMunit(loc))
    expectEquals(expectedSource, optimizedSource, "optimized source")(using toMunit(loc))

  def toMunit(loc: Location) =
    munit.Location(loc.file, loc.line)

  // Algo has no tests

  test("Algo_calc"):
    import Algo_calc.*
    import Algo_calc.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._2, test._1, test._3)(using toMunit(test._4)))

  test("Algo_calc_bindings"):
    import Algo_calc_bindings.*
    import Algo_calc_bindings.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._2, test._1, test._3)(using toMunit(test._4)))

  // Bindings has no tests
  // Blocks has no tests

  test("Calc"):
    import Calc.*
    import Calc.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._2, test._1, test._3)(using toMunit(test._4)))

  test("Calc_bool"):
    import Calc_bool.*
    import Calc_bool.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._2, test._1, test._3)(using toMunit(test._4)))

  test("Calc_int"):
    import Calc_int.*
    import Calc_int.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._2, test._1, test._3)(using toMunit(test._4)))

  // Empty has no tests

  test("Imperative"):
    import Imperative.*
    import Imperative.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._2, test._1, test._3)(using toMunit(test._4)))

  // References has no tests

  test("Optimizer"):
    import Optimizer.*
    val (eOpt, sOpt) = opt(Eval(), ToString())
    testcases.foreach(runTestCaseOpt[Value, Lang](_, Eval(), ToString(), eOpt, sOpt))
