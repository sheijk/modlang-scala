package me
package modlang
package union_interpreter

type Value = IntValue | BoolValue
case class IntValue(v : Int)
case class BoolValue(v : Boolean)

case class Expr(x : ExprT[Expr])
case class And[Expr](lhs : Expr, rhs : Expr)
case class Plus[Expr](lhs : Expr, rhs : Expr)
case class GreaterThan[Expr](lhs : Expr, rhs : Expr)
type ExprT[SubExpr] = Value | And[SubExpr] | Plus[SubExpr] | GreaterThan[SubExpr]

def interprete(e : Expr) : Value =
  e match
    case Expr(IntValue(v)) => IntValue(v)
    case Expr(BoolValue(value)) => BoolValue(value)
    case Expr(And(lhs, rhs)) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (BoolValue(lb), BoolValue(rb)) =>
          BoolValue(lb && rb)
        case _ => throw Error("Expected types Expr.And(bool, bool)")

    case Expr(Plus(lhs, rhs)) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (IntValue(li), IntValue(lr)) =>
          IntValue(li + lr)
        case _ => throw Error("Expected types Expr.Plus(int, int)")

    case Expr(GreaterThan(lhs, rhs)) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (IntValue(li), IntValue(lr)) =>
          BoolValue(li > lr)
        case _ => throw Error("Expected types Expr.Plus(int, int)")

def run(e : Expr) =
  val value = interprete(e)
  println(s"Got $value by running $e")

