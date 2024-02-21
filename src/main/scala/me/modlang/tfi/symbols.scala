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
  trait PushDown[T, Payload] extends Lang[Payload => T], Empty.PushDown[T, Payload, Lang[T]]:
    def bool(value: Boolean) : Payload => l.Expr =
      ctx => l.bool(value)

    def and(lhs: Expr, rhs: Expr): Expr =
      ctx =>
        l.and(lhs(ctx), rhs(ctx))

  given pushDown[T, Payload](using l2: Lang[T]) : PushDown[T, Payload] with {}

// package Imperative:
//   trait PushDown[T](val next: Lang[T]) extends Lang[Payload => T], Empty.PushDown[T, Payload]:
    // def int(v: Int): Expr
    // def plus(lhs: Expr, rhs: Expr): Expr

    // def greaterThan(lhs: Expr, rhs: Expr): Expr

    // def block(statements: Expr*): Expr

    // def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr
    // def loop(name: String, body: Loop => Expr): Expr
    // def break(loop: Loop, ret: Expr): Expr

    // def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr

    // def let(name: String, value: Expr, in: Expr => Expr): Expr

    // def dummy(msg: String, e: Expr): Expr

package Symbols:
  // trait Lang[T] extends Empty.Lang[T]
  // 
  // transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]
  // 
  // trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]
  // 
  // trait ToStringMixin extends Lang[String], EvalId[String]
  // given ToStringMixin with EvalId[String] with {}
  // 
  // trait EvalMixin[T] extends Lang[T], EvalFn[T]

  def demo() =
    println("Symbols & Context")
    import Calc_bool.{*, given}

    // import CaptureLocation.f
    type Ast = [T] => (l: Lang[T]) => l.Expr
    val ast = [T] => (l: Lang[T]) => l.and(l.bool(true), l.bool(false))
    def run[T](ast: Ast)(using l : Lang[T]) = l.eval(ast(l))
    val src: String = run(ast)
    // case class Ctx(value: Int)
    // val ctx: Ctx = Ctx(1)
    type Ctx = Unit
    val ctx: Ctx = ()
    val r: Ctx => Boolean = run(ast)
    println(s"  $src => ${r(ctx)}")
