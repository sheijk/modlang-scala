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

  trait EvalMixin[T] extends Lang[T], EvalHasBool[T]:
    type Expr = () => T
    def if_(cond: () => T, onTrue: Expr, onFalse: Expr): Expr =
      () => if asBool(cond) then onTrue() else onFalse()

