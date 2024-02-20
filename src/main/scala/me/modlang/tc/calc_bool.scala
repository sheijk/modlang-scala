package me
package modlang
package tc

package Calc_bool:
  trait Lang[T] extends Empty.Lang[T]:
    def bool(v: Boolean): Expr
    def and(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String], EvalId[String]:
    type Expr = String

    def bool(v: Boolean): String = v.toString()
    def and(lhs: String, rhs: String): String = s"($lhs & $rhs)"
  given ToStringMixin with {}

  type Value = Boolean
  trait EvalMixin[T] extends Lang[T], EvalHasBool[T]:
    def bool(v: Boolean): Expr = fromBool(v)
    def and(lhs: Expr, rhs: Expr): Expr = fromBool(asBool(lhs) & asBool(rhs))
  given EvalMixin[Value] with EvalId[Value] with EvalBool with {}

  def testcases =
    import CaptureLocation.f
    List(
      f(true, [T] => (l: Lang[T]) => l.bool(true)),
      f(true, [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(true))),
      f(false, [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(false))),
    )
