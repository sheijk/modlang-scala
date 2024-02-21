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

package Imperative:
  trait PushDown[T, Payload, L <: Lang[T]] extends
      Lang[Payload => T],
      Calc_bool.PushDown[T, Payload, L],
      Calc_int.PushDown[T, Payload, L],
      Empty.PushDown[T, Payload, Lang[T]]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = ???

    def block(statements: Expr*): Expr = ???

    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr = ???
    def loop(name: String, body: Loop => Expr): Expr = ???
    def break(loop: Loop, ret: Expr): Expr = ???

    def mut(name: String, value: Expr, in: References.Ref[Expr] => Expr): Expr = ???

    def let(name: String, value: Expr, in: Expr => Expr): Expr = ???

    def dummy(msg: String, e: Expr): Expr = ???

  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload, Lang[T]] with {}

package Symbols:
  type Ast[L[_] <: Empty.Lang[?]] = [T] => (l: L[T]) => l.Expr

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
    import Imperative.{*, given}
    def extract[T](ast: Ast[Lang])(using l : Lang[T]) = l.eval(ast(l))
    type Ctx = Unit
    val ctx = ()
    def run(ex : Ast[Lang]) =
      val src = extract[String](ex)
      val r = extract[Ctx => Value](ex)(ctx)
      println(s"  $src => $r")
    run([T] => (l: Lang[T]) => l.plus(l.int(10), l.int(20)))

  def demo() =
    println("Symbols & Context")
    demoBool()
    demoImperative()
