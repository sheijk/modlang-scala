package me
package modlang
package tfi

package Algo_calc_bindings:
  trait Lang[T] extends Bindings.Lang[T], Algo_calc.Lang[T]

  transparent trait Nested[T, Inner <: Lang[T]] extends
    Lang[T],
    Bindings.Nested[T, Inner],
    Algo_calc.Nested[T, Inner]

  trait Dup[T, L <: Lang[T]] extends
    Lang[T],
    Bindings.Dup[T, L],
    Algo_calc.Dup[T, L]

  trait ToStringMixin extends Lang[String], Algo_calc.ToStringMixin, Bindings.ToStringMixin
  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T], Bindings.EvalMixin[T] , Algo_calc.EvalMixin[T]

  type Value = Algo_calc.Value
  class Eval extends Lang[Value], EvalMixin[Value], EvalFnIntBool[Value]
  given Eval()

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

  object Test:
    type Ast[L[_] <: Empty.Lang[?]] = [T] => (l: L[T]) => l.Expr

    def int(v: Int): Ast[Calc_int.Lang] = [T] => (l: Calc_int.Lang[T]) => l.int(v)
    def let(name: String, value: Ast[Lang], in: [T] => (l: Lang[T]) => Ast[Lang] => Ast[Lang]): Ast[Lang] =
      [T] => (l: Lang[T]) =>
        def body(v: l.Expr): l.Expr = int(0)(l);
        l.let(name, value(l), body)

    def check(ex: Ast[Lang]) =
      val source: String = ex(ToString())
      def eval(l: Lang[Value], ex: Ast[Lang]): Value = l.eval(ex(l))
      val result: Value = eval(Eval(), ex)
      println(s"eval($source) = $result")

    check(int(10))
    check(let("foo", int(10), [T] => (l: Lang[T]) => (foo : Ast[Lang]) => foo))
