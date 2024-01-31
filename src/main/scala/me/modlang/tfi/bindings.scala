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
