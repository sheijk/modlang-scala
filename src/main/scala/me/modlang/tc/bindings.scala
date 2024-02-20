package me
package modlang
package tc

package Bindings:
  trait Lang[T] extends Empty.Lang[T]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      s"let $name = $value in ${in(name)}"

  given ToStringMixin with EvalId[String] with {}

  trait EvalMixin[T] extends Lang[T]:
    type Expr = () => T
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      () => in(value)()
