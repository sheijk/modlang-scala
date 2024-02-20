package me
package modlang
package tfi

package Calc:
  trait Lang[T] extends Calc_int.Lang[T], Calc_bool.Lang[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Calc_int.Nested[T, Inner], Calc_bool.Nested[T, Inner]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Calc_int.Dup[T, L], Calc_bool.Dup[T, L]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (left.greaterThan(lhs._1, rhs._1), right.greaterThan(lhs._2, rhs._2))

  trait ToStringMixin extends Lang[String], Calc_bool.ToStringMixin, Calc_int.ToStringMixin:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = s"($lhs > $rhs)"
  given ToStringMixin()

  type Value = Int | Boolean
  trait EvalMixin[T] extends Lang[T], Calc_bool.EvalMixin[T], Calc_int.EvalMixin[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr =
      () => fromBool(asInt(lhs()) > asInt(rhs()))
  given EvalMixin[Value] with EvalIntBool[Value] with {}

  type MyTest = TestLoc[Value, Lang]
  def testcases: List[MyTest] =
    import CaptureLocation.f
    Calc_int.testcases.asInstanceOf[List[MyTest]] ++
    Calc_bool.testcases.asInstanceOf[List[MyTest]] ++
    List(
        f(true,
          [T] => (l: Lang[T]) =>
            l.and(
              l.greaterThan(l.int(10), l.int(5)),
              l.greaterThan(l.int(3), l.int(2)))),
      )
  type Program = [T] => (l: Lang[T]) => l.Expr
