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

  def toMunit(loc: Location) =
    munit.Location(loc.file, loc.line)

  test("Imperative"):
    import Imperative.*
    import Imperative.given
    testcases.map(runTestCase[Value, Lang]).foreach(test =>
      expectEquals(test._1, test._2, test._3)(using toMunit(test._4)))
