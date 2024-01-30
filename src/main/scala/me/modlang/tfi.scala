package me
package modlang
package tfi

import me.modlang.tfi.Calc_bool.Lang

// consider adding more types, like TypedExpr[T], etc.

object Calc_bool:
  trait Lang:
    type Value
    type Expr <: Value
    def bool(v: Boolean): Expr
    def &(lhs: Expr, rhs: Expr): Expr

  class ToString extends Lang:
    type Value = String
    type Expr = String
    override def bool(v: Boolean): Value = v.toString()
    override def &(lhs: Expr, rhs: Expr): Expr = s"($lhs & $rhs)"

object Calc_int:
  trait Lang:
    type Value
    type Expr <: Value
    def int(v: Int): Value
    def +(lhs: Expr, rhs:Expr):Expr

  class ToString extends Lang:
    type Value = String
    type Expr = String

    override def int(v: Int): Value = v.toString()
    override def +(lhs: Expr, rhs: Expr): Expr = s"($lhs + $rhs)"

object Calc:
  trait Lang extends Calc_bool.Lang, Calc_int.Lang:
    def <(lhs:Expr, rhs:Expr):Expr

