package me
package modlang

trait Ast[T]:
  type Value
  def int(v: Int): Value
  def bool(b: Boolean): Value
  def c(v : Value) : T
  def c(v : Int) : T = c(int(v))
  def c(v : Boolean) : T = c(bool(v))
  def plus(lhs: T, rhs: T): T
  def and(lhs: T, rhs: T): T
  def greaterThan(lhs: T, rhs: T): T

trait Runner[Expr]:
  type Value
  def name: String
  def run(e: Expr): Value
  def runAndPrint(e: Expr) =
    val value = run(e)
    println(s"Got $value by running $e")

class AdtAst extends Ast[adt_interpreter.Expr], Runner[adt_interpreter.Expr]:
  import adt_interpreter.*

  type Value = adt_interpreter.Value
  def int(v: Int) = Value.I(v)
  def bool(v: Boolean) = Value.B(v)
  def c(v: Value) = Expr.Constant(v)
  def plus(lhs: Expr, rhs: Expr) = Expr.Plus(lhs, rhs)
  def and(lhs: Expr, rhs: Expr) = Expr.And(lhs, rhs)
  def greaterThan(lhs: Expr, rhs: Expr) = Expr.GreaterThan(lhs, rhs)

  type Result = Value
  def name = "adt interpreter"
  def run(e: Expr) = interprete(e)

class UnionAst extends Ast[union_interpreter.Expr], Runner[union_interpreter.Expr]:
  import union_interpreter.*

  type Value = union_interpreter.Value
  def int(v: Int) = IntValue(v)
  def bool(v: Boolean) = BoolValue(v)
  def c(v : Value) = Expr(v)
  def plus(lhs: Expr, rhs: Expr) = Expr(Plus(lhs, rhs))
  def and(lhs: Expr, rhs: Expr) = Expr(And(lhs, rhs))
  def greaterThan(lhs: Expr, rhs: Expr) = Expr(GreaterThan(lhs, rhs))

  type Result = Value
  def name = "union interpreter"
  def run(e: Expr) = interprete(e)
