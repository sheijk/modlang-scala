package me
package modlang

abstract class InterpreterSuite[Expr](a : Ast[Expr] & Runner[Expr]) extends TestSuite:
  import a.*

  test("simple value equality"):
    expectEquals(int(10), int(10))
    expectEquals(bool(true), bool(true))
    expectEquals(bool(false), bool(false))

  test("different values should be different"):
    expectNotEquals(bool(true), bool(false))
    expectNotEquals(int(1), int(2))

  private def value(v : Int | Boolean) =
    v match
    case v : Int => int(v)
    case v : Boolean => bool(v)

  def checkEval(expected: Int|Boolean, program: Expr)(implicit loc : munit.Location) =
    expectEquals(value(expected), run(program))

  test("simple expression evaluation"):
    checkEval(3, plus(c(1), c(2)))
    checkEval(15, plus(c(10), c(5)))
    checkEval(true, and(greaterThan(c(10), c(5)), greaterThan(c(3), c(2))))

  test("and"):
    checkEval(true, and(c(true), c(true)))
    checkEval(false, and(c(true), c(false)))
    checkEval(false, and(c(false), c(true)))
    checkEval(false, and(c(false), c(false)))

// final class AdtInterpreterSuite extends InterpreterSuite[adt_interpreter.Expr](new AdtAst())
// final class UnionInterpreterSuite extends InterpreterSuite[union_interpreter.Expr](new UnionAst())


