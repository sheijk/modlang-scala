package me
package modlang
package tc

package Calc:
  trait Lang[T] extends Calc_int.Lang[T], Calc_bool.Lang[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String], Calc_bool.ToStringMixin, Calc_int.ToStringMixin:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = s"($lhs > $rhs)"
  given ToStringMixin()

  type Value = Int | Boolean
  trait EvalMixin[T] extends Lang[T], Calc_bool.EvalMixin[T], Calc_int.EvalMixin[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = fromBool(asInt(lhs) > asInt(rhs))
  given EvalMixin[Value] with EvalId[Value] with EvalIntBool[Value] with {}

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
