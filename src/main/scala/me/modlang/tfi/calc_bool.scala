package me
package modlang
package tfi

package Calc_bool:
  trait Lang[T] extends Empty.Lang[T]:
    def bool(v: Boolean): Expr
    def and(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String], EvalId[String]:
    type Expr = String

    def bool(v: Boolean): String = v.toString()
    def and(lhs: String, rhs: String): String = s"($lhs & $rhs)"

  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T], EvalHasBool[T]:
    def bool(v: Boolean): Expr = fromBool(v)
    def and(lhs: Expr, rhs: Expr): Expr = fromBool(asBool(lhs) & asBool(rhs))

  type Value = Boolean

  class Eval extends EvalMixin[Value], EvalId[Value], EvalBool
  given Eval()

  def testcases =
    import CaptureLocation.f
    List(
      f(true, [T] => (l: Lang[T]) => l.bool(true)),
      f(true, [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(true))),
      f(false, [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(false))),
    )
