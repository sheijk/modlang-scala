package me
package modlang
package tfi

package Optimizer:
  // class ConstantFoldInt[T, Nested <: Calc_int.Lang[T]] extends Calc_int.Lang[T]:
  //   case class Expr(inner: Nested.Expr)
  //   override def int(v: Int): Expr = ???
  //   override def plus(lhs: Expr, rhs: Expr): Expr = ???
  // 
  //   override def eval(e: Expr): Result = ???

  case class ConstantFoldInt[T, Nested <: Calc_int.Lang[T]](inner: Nested) extends Calc_int.Lang[T]:
    enum Expr:
      case Constant(i: Int)
      case Dynamic(i: inner.Expr)

      def mapInt(f: Int => Expr) =
        this match
          case Constant(i) => f(i)
          case Dynamic(_) => this

    def mapIntInt(lhs: Expr, rhs: Expr, f: (Int, Int) => Expr) =
      lhs.mapInt(lhsValue => rhs.mapInt(rhsValue => f(lhsValue, rhsValue)))

    override def int(v: Int): Expr = Expr.Constant(v)
    override def plus(lhs: Expr, rhs: Expr): Expr =
      mapIntInt(lhs, rhs, (lhs, rhs) => Expr.Constant(lhs + rhs))

    override def eval(e: Expr): Result =
