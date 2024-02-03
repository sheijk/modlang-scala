package me
package modlang
package tfi

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

  def runTestCaseX[Value, Lang[_] <: Empty.Lang[?]](
    t: ([T] => (l: Lang[T]) => l.Expr, [T] => (l: Lang[T]) => l.Expr, Location),
    s: Lang[String],
    e: Lang[Value]
  ): (e.Result, e.Result, s.Result, Location) =
    // val expected = t._1
    // val program = t._2
    // val source = s.eval(program(s))
    // val result = e.eval(program(e))
    val expected = t._1
    val program = t._2
    val expectedValue = e.eval(expected(e))
    val source = s.eval(program(s))
    val result = e.eval(program(e))
    (expectedValue, result, source, t._3)

  def toMunit(loc: Location) =
    munit.Location(loc.file, loc.line)

  // Algo has no tests

  test("Algo_calc"):
    import Algo_calc.*
    import Algo_calc.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))

  test("Algo_calc_bindings"):
    import Algo_calc_bindings.*
    import Algo_calc_bindings.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))

  // Bindings has no tests
  // Blocks has no tests

  test("Calc"):
    import Calc.*
    import Calc.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))

  test("Calc_bool"):
    import Calc_bool.*
    import Calc_bool.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))

  test("Calc_int"):
    import Calc_int.*
    import Calc_int.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))

  // Empty has no tests

  test("Imperative"):
    import Imperative.*
    import Imperative.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))

  // References has no tests

  test("Optimized programs still work"):
    import Optimizer.*
    def runWith[Value](l: (Lang[Value], Lang[String])) =
      val e : Lang[Value] = l._1
      val s : Lang[String] = l._2
      def runTest(expected: [T2] => (l: Lang[T2]) => l.Expr, f: [T2] => (l: Lang[T2]) => l.Expr, loc: Location) =
        runTestCaseX[Value, Lang]((expected, f, loc), s, e)
      testcases.map(runTest).foreach(test =>
        expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))
    // Check that the expression will be optimized
    runWith(opt(Calc.ToString(), Calc.ToString()))
    // Check that running it produces the correct result
    runWith(opt(Calc.Eval(), Calc.ToString()))

  // test("Optimized programs still work"):
  //   import Optimizer.*
  //   def run[T](e : Lang[T], s: Lang[String]) =
  //     // given e : Lang[T] = l._
  //     // given s : Lang[String] = l._2
  //     given e : Lang[T] = e
  //     given s : Lang[String] = s
  //     testcases.map(runTestCase[T, Lang]).foreach(test =>
  //       expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))
  //   val l = opt(Calc.ToString(), Calc.ToString())
  //   run(l._1, l._2)
