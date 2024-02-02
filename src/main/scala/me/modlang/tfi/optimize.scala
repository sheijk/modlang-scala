package me
package modlang
package tfi

package Optimizer:
  case class ConstantFoldInt[T, Nested <: Calc_int.Lang[T]](inner: Nested) extends Calc_int.Lang[T]:
    type Expr = Either[inner.Expr, Int]

    def mapIntInt(lhs: Expr, rhs: Expr, f: (Int, Int) => Int): Expr =
      lhs.flatMap(lhsValue => rhs.map(rhsValue => f(lhsValue, rhsValue)))

    override def int(v: Int): Expr = Right(v)
    override def plus(lhs: Expr, rhs: Expr): Expr =
      mapIntInt(lhs, rhs, (lhs, rhs) => lhs + rhs)

    override def eval(e: Expr): Result =
      val innerExpr = e.match
        case Left(dynamic) => dynamic
        case Right(i) => inner.int(i)
      inner.eval(innerExpr)
