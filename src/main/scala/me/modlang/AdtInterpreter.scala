package me
package modlang
package adt_interpreter

enum Value:
  case I(value : Int)
  case B(value : Boolean)

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
        case (Value.B(lb), Value.B(rb)) =>
          Value.B(lb && rb)
        case _ => throw Error("Expected types Expr.And(bool, bool)")

    case Expr.Plus(lhs, rhs) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (Value.I(li), Value.I(lr)) =>
          Value.I(li + lr)
        case _ => throw Error("Expected types Expr.Plus(int, int)")

    case Expr.GreaterThan(lhs, rhs) =>
      val lhsValue = interprete(lhs)
      val rhsValue = interprete(rhs)
      (lhsValue, rhsValue) match
        case (Value.I(li), Value.I(lr)) =>
          Value.B(li > lr)
        case _ => throw Error("Expected types Expr.Plus(int, int)")

def run(e : Expr) =
  val value = interprete(e)
  println(s"Got $value by running $e")

