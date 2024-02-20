package me
package modlang
package tfi

package Empty:
  trait PushDown[T, Payload] extends Lang[Payload => T]

package Calc_bool:
  case class Payload(value: Int)
  trait PushDown[T](val next: Lang[T]) extends Lang[Payload => T], Empty.PushDown[T, Payload]:
    val l = next
    type Expr = Payload => l.Expr

    def bool(value: Boolean) : Payload => l.Expr = ctx => l.bool(value)
    def and(lhs: Expr, rhs: Expr): Expr =
      ctx =>
        l.and(lhs(ctx), rhs(ctx))
        l.and(lhs(ctx), rhs(ctx))

    // def eval(e: Expr): Result = l.eval(e(l)(Payload(0)))
    def eval(e: Expr): Payload => Result =
      val x: l.Expr = e(Payload(0))
      val r: T = l.eval(x)
      r

    // l.eval(e(Payload(0)))
  given pushDown[T](using l: Lang[T]) : PushDown[T](l) with {}

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
    val ast = [T] => (l: Lang[T]) => l.bool(true)
    // runProgram[Boolean, Lang](ast)
    def run[T](ast: Ast)(using l : Lang[T]) = l.eval(ast(l))
    val src: String = run(ast)
    val r: Payload => Boolean = run(ast)
    println(s"$src => $r")
