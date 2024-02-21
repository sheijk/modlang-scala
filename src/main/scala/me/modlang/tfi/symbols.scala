package me
package modlang
package tfi

package Empty:
  trait PushDown[T, Payload, L <: Lang[T]](using val next: L) extends Lang[Payload => T]:
    val l = next
    type Expr = Payload => l.Expr

    def eval(e: Expr): Result =
      (ctx: Payload) => l.eval(e(ctx))

package Calc_bool:
  trait PushDown[T, Payload, L <: Lang[T]] extends Lang[Payload => T], Empty.PushDown[T, Payload, L]:
    def bool(value: Boolean) : Payload => l.Expr =
      ctx => l.bool(value)

    def and(lhs: Expr, rhs: Expr): Expr =
      ctx =>
        l.and(lhs(ctx), rhs(ctx))

  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload, Lang[T]] with {}

package Calc_int:
  trait PushDown[T, Payload, L <: Lang[T]] extends Lang[Payload => T], Empty.PushDown[T, Payload, L]:
    def int(value: Int) : Payload => l.Expr =
      ctx => l.int(value)

    def plus(lhs: Expr, rhs: Expr): Expr =
      ctx =>
        l.plus(lhs(ctx), rhs(ctx))

  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload, Lang[T]] with {}

package Algo_calc:
  trait PushDown[T, Payload, L <: Lang[T]] extends
      Lang[Payload => T],
      Calc_bool.PushDown[T, Payload, L],
      Calc_int.PushDown[T, Payload, L]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = ???

    def block(statements: Expr*): Expr = ???

    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr = ???
    def loop(name: String, body: Loop => Expr): Expr = ???
    def break(loop: Loop, ret: Expr): Expr = ???

    // def mut(name: String, value: Expr, in: References.Ref[Expr] => Expr): Expr = ???

    // def let(name: String, value: Expr, in: Expr => Expr): Expr = ???

    // def dummy(msg: String, e: Expr): Expr = ???

  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload, Lang[T]] with {}

type Ast[L[_] <: Empty.Lang[?]] = [T] => (l: L[T]) => l.Expr

package Scope:
  class Scope:
    var table = Map[String, Any]()
    def lookup(id: String): Option[Any] =
      table get id
    def register(id: String, obj: Any): Unit =
      table = table + (id -> obj)

  trait Lang[T] extends Empty.Lang[T]:
    def let(name: String, value: Expr, body: Expr): Expr
    def get(name: String): Expr

  trait ToStringMixin extends Lang[String]:
    type Expr = Result
    def let(name: String, value: Expr, body: Expr): Expr =
      s"(let $name = $value in $body)"
    def get(name: String): Expr =
      s"get($name)"
  given ToStringMixin with EvalId[String] with {}

  trait EvalMixin[T] extends Lang[T], EvalHasBool[T], EvalFn[T]:
    var topScope = Scope()
    def let(name: String, value: Expr, body: Expr): Expr =
      () =>
        val prevScope = topScope
        topScope = Scope()
        topScope.register(name, value())
        val bodyResult = body()
        topScope = prevScope
        bodyResult
    def get(name: String): Expr =
      () =>
        topScope.lookup(name) match
          case Some(v) => v.asInstanceOf[T]
          case _ => throw Error(s"could not find variable $name")

  trait PushDown[T, Payload, L <: Lang[T]] extends
      Lang[Payload => T],
      Empty.PushDown[T, Payload, L]:
    def let(name: String, value: Expr, body: Expr): Expr =
      ctx => l.let(name, value(ctx), body(ctx))
    def get(name: String): Expr =
      ctx => l.get(name)
  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload, Lang[T]] with {}

package Scoped_algo:
  trait Lang[T] extends Algo_calc.Lang[T], Scope.Lang[T]

  // transparent trait Nested[T, Inner <: Lang[T]] extends
  //   Lang[T],
  //   Algo_calc.Nested[T, Inner],
  //   Scope.Nested[T, Inner]
  // 
  // trait Dup[T, L <: Lang[T]] extends
  //   Lang[T],
  //   Algo.Dup[T, L],
  //   Calc.Dup[T, L]

  trait ToStringMixin extends Lang[String], Algo_calc.ToStringMixin, Scope.ToStringMixin
  given ToStringMixin()

  type Value = Calc.Value
  trait EvalMixin[T] extends Lang[T] , Algo_calc.EvalMixin[T] , Scope.EvalMixin[T]
  given EvalMixin[Value] with EvalIntBool[Value] with {}

  trait PushDown[T, Payload, L <: Lang[T]] extends
      Lang[Payload => T],
      Algo_calc.PushDown[T, Payload, L],
      Scope.PushDown[T, Payload, L]
  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload, Lang[T]] with {}

  def testcases =
    import CaptureLocation.f
    List(
      f(11,
      [T] => (l: Lang[T]) =>
        import l.*
        let("foo", int(10), plus(int(1), get("foo")))),
    )

package Symbols:
  def demoBool() =
    import Calc_bool.{*, given}

    val ast = [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(false))
    def run[T](ast: Ast[Lang])(using l : Lang[T]) = l.eval(ast(l))
    val src: String = run(ast)
    // case class Ctx(value: Int)
    // val ctx: Ctx = Ctx(1)
    type Ctx = Unit
    val ctx: Ctx = ()
    val r: Ctx => Boolean = run(ast)
    println(s"  $src => ${r(ctx)}")

  def demoImperative() =
    import Scoped_algo.{*, given}
    def extract[T](ast: Ast[Lang])(using l : Lang[T]) = l.eval(ast(l))
    type Ctx = Unit
    val ctx = ()
    def run(ex : Ast[Lang]) =
      val src = extract[String](ex)
      val r = extract[Ctx => Value](ex)(ctx)
      println(s"  $src => $r")
    run([T] => (l: Lang[T]) => l.plus(l.int(10), l.int(20)))
    testcases.map(_._2).foreach(run)

  def demo() =
    println("Symbols & Context")
    demoBool()
    demoImperative()
