package me
package modlang
package tfi

package Bindings:
  trait Lang[T] extends Empty.Lang[T]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      s"let $name = $value in ${in(name)}"

  class ToString extends ToStringMixin, EvalId[String]
  given ToString()

  trait EvalMixin[T] extends Lang[T]:
    type Expr = () => T
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      () => in(value)()

    def fromBool(v: Boolean): Expr
    def asBool(t: Expr): Boolean

package Algo_calc_bindings:
  trait Lang[T] extends Bindings.Lang[T], Algo_calc.Lang[T]

  trait ToStringMixin extends Lang[String], Algo_calc.ToStringMixin, Bindings.ToStringMixin
  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T], Bindings.EvalMixin[T] , Algo_calc.EvalMixin[T]

  type Value = Algo_calc.Value
  class Eval extends EvalMixin[Value], EvalFn[Value]
  given Eval()

  def tests() =
    List(
      (108,
      [T] => (l: Lang[T]) =>
        import l.*
        let("x", plus(int(5), int(3)), x =>
          plus(int(100), x))),
      (20,
      [T] => (l: Lang[T]) => l.plus(l.int(5), l.int(15))),
      (321,
      [T] => (l: Lang[T]) =>
        import l.*
        let("x", plus(int(20), int(1)), x =>
        let("y", plus(int(100), int(200)), y =>
        plus(x, y)))),
    )

  def testing() = tests().foreach(runTest[Value, Lang])
