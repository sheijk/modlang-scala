package me
package modlang
package tfi

package Lambda:
  trait Lang[T] extends Empty.Lang[T]:
    def fn(name: String, body: Expr => Expr, in: (Expr => Expr) => Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    def fn(name: String, body: Expr => Expr, in: (Expr => Expr) => Expr): Expr

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    def fn(name: String, body: Expr => Expr, in: (Expr => Expr) => Expr): Expr = ???

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def fn(name: String, body: Expr => Expr, in: (Expr => Expr) => Expr): Expr =
      val bodyStr = body("arg")
      val inStr = in(arg => s"$name($arg)")
      s"(fn $name = $bodyStr :in $inStr)"

  class ToString extends ToStringMixin, EvalId[String]
  given ToString()

  trait EvalMixin[T] extends Lang[T], EvalFn[T]:
    def fn(name: String, body: Expr => Expr, in: (Expr => Expr) => Expr): Expr =
      () => in(arg => () => body(arg)())()
