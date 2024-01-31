package me
package modlang
package tfi

package Imperative:
  trait Lang[T] extends References.Lang[T], Algo_calc_bindings.Lang[T], Blocks.Lang[T]

  trait ToStringMixin extends Lang[String], Algo_calc_bindings.ToStringMixin, References.ToStringMixin, Blocks.ToStringMixin
  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T], References.EvalMixin[T] , Algo_calc_bindings.EvalMixin[T], Blocks.EvalMixin[T]

  type Value = Algo_calc_bindings.Value
  class Eval extends EvalMixin[Value], EvalFnIntBool[Value]
  given Eval()

  def tests() =
    List(
      (3,
      [T] => (l: Lang[T]) =>
        import l.*
        block(int(1), int(2), int(3))),
      (10,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("x", int(10), x => x.get())),
      (20,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("x", int(5), x =>
        block(
          x.set(plus(int(15), int(5)))
        ))),
      (11,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("foo", int(1), foo =>
        block(
          foo.set(int(666)),
          foo.set(plus(foo.get(), int(10))),
          foo.get()
        ))),
    )

  def testing() = tests().foreach(runTest[Value, Lang])
