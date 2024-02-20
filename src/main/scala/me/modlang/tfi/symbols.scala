package me
package modlang
package tfi

package Empty:
  type Context[Ctx, T] = (Ctx, T)
  trait PushDown[T, Payload] extends Lang[Payload => T]

package Imperative:
  type Payload = Int
  trait PushDown[T](val next: Lang[T]) extends Lang[Payload => T], Empty.PushDown[T, Payload]:
    val l = next
    type Expr = Payload => l.Expr

    def bool(value: Boolean) : Payload => l.Expr = ctx => l.bool(value)
    def and(lhs: Expr, rhs: Expr): Expr =
      ctx =>
        l.and(lhs(ctx), rhs(ctx))
        l.and(lhs(ctx), rhs(ctx))

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
  trait Lang[T] extends Empty.Lang[T]

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]

  trait ToStringMixin extends Lang[String], EvalId[String]
  given ToStringMixin with EvalId[String] with {}

  trait EvalMixin[T] extends Lang[T], EvalFn[T]
