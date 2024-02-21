package me
package modlang
package macro_compiler

type Value = Int | Boolean

enum Expr:
  case Constant(value : Value)
  case And(lhs : Expr, rhs : Expr)
  case Plus(lhs : Expr, rhs : Expr)
  case GreaterThan(lhs : Expr, rhs : Expr)

def interprete(e : Expr) : Value =
  e match
    case Expr.Constant(value) => value
    case Expr.And(lhs, rhs) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (lb : Boolean, rb : Boolean) =>
          lb && rb
        case _ => throw Error("Expected types Expr.And(bool, bool)")

    case Expr.Plus(lhs, rhs) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (li: Int, lr: Int) =>
          li + lr
        case _ => throw Error("Expected types Expr.Plus(int, int)")

    case Expr.GreaterThan(lhs, rhs) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (li: Int, lr: Int) =>
          li > lr
        case _ => throw Error("Expected types Expr.Plus(int, int)")

def run(e : Expr) =
  val value = interprete(e)
  println(s"  Got $value by running $e")

class Builder:
  def int(v: Int): Value = v
  def bool(v: Boolean): Value = v
  def c(v: Value): Expr = Expr.Constant(v)
  def c(v : Int): Expr = c(int(v))
  def c(v : Boolean): Expr = c(bool(v))
  def plus(lhs: Expr, rhs: Expr) = Expr.Plus(lhs, rhs)
  def and(lhs: Expr, rhs: Expr) = Expr.And(lhs, rhs)
  def greaterThan(lhs: Expr, rhs: Expr) = Expr.GreaterThan(lhs, rhs)

def demo() =
  val builder = Builder()
  import builder.*
  val programs = List(
    plus(c(10), c(5)),
    and(greaterThan(c(10), c(5)), greaterThan(c(3), c(2))),
    and(c(true), c(false)),
    and(greaterThan(c(10), c(5)), greaterThan(c(10), c(5))),
    plus(c(10), c(20)),
  )
  def run(ex: Expr) =
    val r = interprete(ex)
    println(s"  $ex = $r")
  programs.foreach(run)

