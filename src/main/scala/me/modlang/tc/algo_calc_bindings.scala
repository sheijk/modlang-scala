package me
package modlang
package tc

package Algo_calc_bindings:
  trait Lang[T] extends Bindings.Lang[T], Algo_calc.Lang[T]

  trait ToStringMixin extends Lang[String], Algo_calc.ToStringMixin, Bindings.ToStringMixin
  given ToStringMixin()

  type Value = Algo_calc.Value
  trait EvalMixin[T] extends Lang[T], Bindings.EvalMixin[T] , Algo_calc.EvalMixin[T]
  given EvalMixin[Value] with EvalIntBool[Value] with {}

  def testcases =
    import CaptureLocation.f
    List(
      f(108,
      [T] => (l: Lang[T]) =>
        import l.*
        let("x", plus(int(5), int(3)), x =>
          plus(int(100), x))),
      f(20,
      [T] => (l: Lang[T]) => l.plus(l.int(5), l.int(15))),
      f(321,
      [T] => (l: Lang[T]) =>
        import l.*
        let("x", plus(int(20), int(1)), x =>
        let("y", plus(int(100), int(200)), y =>
        plus(x, y)))),
    )
