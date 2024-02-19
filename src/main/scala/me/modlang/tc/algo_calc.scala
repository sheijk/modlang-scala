package me
package modlang
package tc

package Algo_calc:
  trait Lang[T] extends Algo.Lang[T], Calc.Lang[T]

  transparent trait Nested[T, Inner <: Lang[T]] extends
    Lang[T],
    Algo.Nested[T, Inner],
    Calc.Nested[T, Inner]

  trait Dup[T, L <: Lang[T]] extends
    Lang[T],
    Algo.Dup[T, L],
    Calc.Dup[T, L]

  trait ToStringMixin extends Lang[String], Algo.ToStringMixin, Calc.ToStringMixin
  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T] , Calc.EvalMixin[T] , Algo.EvalMixin[T]

  type Value = Calc.Value
  class Eval extends EvalMixin[Value], EvalFnIntBool[Value]
  given Eval()

  def testcases =
    import CaptureLocation.f
    List(
      f(true,
      [T] =>
        (l: Lang[T]) =>
          l.and(l.greaterThan(l.int(10), l.int(5)), l.greaterThan(l.int(3), l.int(2)))),
      f(10,
      [T] =>
        (l: Lang[T]) =>
          l.if_(
            l.greaterThan(l.int(10), l.int(5)),
            l.plus(l.int(6), l.int(4)),
            l.int(-1)
          )
        )
    )
