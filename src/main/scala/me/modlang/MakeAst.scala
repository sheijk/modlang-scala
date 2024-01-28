package me
package modlang

trait Ast[T]:
  def int(v: Int): T
  def bool(b: Boolean): T
  def plus(lhs: T, rhs: T): T
  def and(lhs: T, rhs: T): T
  def greaterThan(lhs: T, rhs: T): T

trait Runner[Expr]:
  type Result
  def name: String
  def run(e: Expr): Result
  def runAndPrint(e: Expr) =
    val value = run(e)
    println(s"Got $value by running $e")

class AdtAst extends Ast[adt_interpreter.Expr], Runner[adt_interpreter.Expr]:
  import adt_interpreter.*

  def int(v: Int) = Expr.Constant(Value.I(v))
  def bool(v: Boolean) = Expr.Constant(Value.B(v))
  def plus(lhs: Expr, rhs: Expr) = Expr.Plus(lhs, rhs)
  def and(lhs: Expr, rhs: Expr) = Expr.And(lhs, rhs)
  def greaterThan(lhs: Expr, rhs: Expr) = Expr.GreaterThan(lhs, rhs)

  type Result = Value
  def name = "adt interpreter"
  def run(e: Expr) = interprete(e)

class UnionAst extends Ast[union_interpreter.Expr], Runner[union_interpreter.Expr]:
  import union_interpreter.*

  def int(v: Int) = Expr(IntValue(v))
  def bool(v: Boolean) = Expr(BoolValue(v))
  def plus(lhs: Expr, rhs: Expr) = Expr(Plus(lhs, rhs))
  def and(lhs: Expr, rhs: Expr) = Expr(And(lhs, rhs))
  def greaterThan(lhs: Expr, rhs: Expr) = Expr(GreaterThan(lhs, rhs))

  type Result = Value
  def name = "union interpreter"
  def run(e: Expr) = interprete(e)
