package me
package modlang

final class InterpreterSuite extends TestSuite:
  import adt_interpreter.*
  test("simple value equality"):
    expectEquals(Value.I(10), Value.I(10))
    expectEquals(Value.B(true), Value.B(true))
    expectEquals(Value.B(false), Value.B(false))

  test("different values should be different"):
    expectNotEquals(Value.B(true), Value.B(false))
    expectNotEquals(Value.I(1), Value.I(2))

  def int(v: Int) = Expr.Constant(Value.I(v))
  def bool(v: Boolean) = Expr.Constant(Value.B(v))
  import Expr.*

  def value(v : Int | Boolean) =
    v match
    case v : Int => Value.I(v)
    case v : Boolean => Value.B(v)

  def checkEval(expected: Int|Boolean, program: Expr)(implicit loc : munit.Location) =
    expectEquals(value(expected), interprete(program))

  test("simple expression evaluation"):
    checkEval(3, Plus(int(1), int(2)))
    checkEval(15, Plus(int(10), int(5)))
    checkEval(true, And(GreaterThan(int(10), int(5)), GreaterThan(int(3), int(2))))

  test("and"):
    checkEval(true, And(bool(true), bool(true)))
    checkEval(false, And(bool(true), bool(false)))
    checkEval(false, And(bool(false), bool(true)))
    checkEval(false, And(bool(false), bool(false)))

