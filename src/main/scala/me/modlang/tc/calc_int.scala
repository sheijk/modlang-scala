package me
package modlang
package tc

package Calc_int:
  trait Lang[T] extends Empty.Lang[T]:
    def int(v: Int): Expr
    def plus(lhs: Expr, rhs: Expr): Expr

  // transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
  //   override def int(v: Int): Expr =
  //     toOuter(inner.int(v))
  // 
  //   override def plus(lhs: Expr, rhs: Expr): Expr =
  //     toOuter(inner.plus(toInner(lhs), toInner(rhs)))
  // 
  // trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
  //   override def int(v: Int): Expr =
  //     (left.int(v), right.int(v))
  // 
  //   override def plus(lhs: Expr, rhs: Expr): Expr =
  //     (left.plus(lhs._1, rhs._1), right.plus(lhs._2, rhs._2))

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def int(v: Int): String = v.toString()
    def plus(lhs: String, rhs: String): String = s"($lhs + $rhs)"
  given ToStringMixin()

  type Value = Int
  trait EvalMixin[T] extends Lang[T], EvalHasInt[T]:
    def int(v: Int): Expr = fromInt(v)
    def plus(lhs: Expr, rhs: Expr): Expr = fromInt(asInt(lhs) + asInt(rhs))
  given EvalMixin[Value] with EvalId[Value] with EvalInt with {}

  def testcases =
    import CaptureLocation.f
    List(
      f(10, [T] => (l: Lang[T]) => l.int(10)),
      f(20, [T] => (l: Lang[T]) => l.plus(l.int(5), l.int(15))),
    )
