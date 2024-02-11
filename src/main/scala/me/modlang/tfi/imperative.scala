package me
package modlang
package tfi

package Imperative:
  trait Lang[T] extends
      References.Lang[T],
      Algo_calc_bindings.Lang[T],
      Blocks.Lang[T],
      Lambda.Lang[T]

  trait Nested[T, Inner <: Lang[T]] extends
      Lang[T],
      References.Nested[T, Inner],
      Algo_calc_bindings.Nested[T, Inner],
      Blocks.Nested[T, Inner],
      Lambda.Nested[T, Inner]

  trait Dup[T, L <: Lang[T]] extends
      Lang[T],
      References.Dup[T, L],
      Algo_calc_bindings.Dup[T, L],
      Blocks.Dup[T, L],
      Lambda.Dup[T, L]

  trait ToStringMixin extends
      Lang[String],
      Algo_calc_bindings.ToStringMixin,
      References.ToStringMixin,
      Blocks.ToStringMixin,
      Lambda.ToStringMixin
  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends
      Lang[T],
      References.EvalMixin[T], Algo_calc_bindings.EvalMixin[T],
      Blocks.EvalMixin[T],
      Lambda.EvalMixin[T]

  type Value = Algo_calc_bindings.Value
  class Eval extends EvalMixin[Value], EvalFnIntBool[Value]
  given Eval()

  def testcases =
    import CaptureLocation.f
    List(
      f(3,
      [T] => (l: Lang[T]) =>
        import l.*
        block(int(1), int(2), int(3))),
      f(10,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("x", int(10), x => x.get())),
      f(20,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("x", int(5), x =>
        block(
          x.set(plus(int(15), int(5)))
        ))),
      f(11,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("foo", int(1), foo =>
        block(
          foo.set(int(666)),
          foo.set(plus(foo.get(), int(10))),
          foo.get()
        ))),
      f(55,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("idx", int(0), idx =>
        mut("sum", int(0), sum =>
        loop("l", l =>
          if_(greaterThan(idx.get(), int(10)),
            break(l, sum.get()),
            block(
              idx.set(plus(idx.get(), int(1))),
              sum.set(plus(sum.get(), idx.get()))
              )))))),
      f(20,
      [T] => (l: Lang[T]) =>
        import l.*
        fn("x2", arg => plus(arg, arg), f =>
          f(int(10)))),
      f(30,
      [T] => (l: Lang[T]) =>
        import l.*
        fn("x2", arg => plus(arg, arg), f =>
          plus(f(int(10)), f(int(5))))),
      f(10,
      [T] => (l: Lang[T]) =>
        import l.*
        mut("counter", int(1), counter =>
        fn("inc", amount => counter.set(plus(counter.get(), amount)), inc =>
          block(
            inc(int(2)),
            inc(int(3)),
            inc(int(4)),
            counter.get())))),
    )
