package me
package modlang
package tfi

package Algo:
  trait Lang[T] extends Empty.Lang[T]:
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr

  trait ToStringMixin extends Lang[String]:
    type Expr = Result
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
      s"(if $cond :then $onTrue :else $onFalse)"

  class ToString extends ToStringMixin, EvalId[String]
  given ToString()

  trait EvalMixin[T] extends Lang[T]:
    type Expr = () => T
    def if_(cond: () => T, onTrue: Expr, onFalse: Expr): Expr =
      () => if asBool(cond) then onTrue() else onFalse()

    def fromBool(v: Boolean): Expr
    def asBool(t: Expr): Boolean

package Algo_calc:
  trait Lang[T] extends Algo.Lang[T], Calc.Lang[T]

  trait ToStringMixin extends Lang[String], Algo.ToStringMixin, Calc.ToStringMixin
  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T] , Calc.EvalMixin[T] , Algo.EvalMixin[T]

  type Value = Calc.Value
  class Eval extends EvalMixin[Value], EvalFn[Value]
  given Eval()

  def tests() =
    List(
      (true,
      [T] =>
        (l: Lang[T]) =>
          l.and(l.greaterThan(l.int(10), l.int(5)), l.greaterThan(l.int(3), l.int(2)))),
      (10,
      [T] =>
        (l: Lang[T]) =>
          l.if_(
            l.greaterThan(l.int(10), l.int(5)),
            l.plus(l.int(6), l.int(4)),
            l.int(-1)
          )
        )
    )

  def testing() = tests().foreach(runTest[Calc.Value, Lang])
