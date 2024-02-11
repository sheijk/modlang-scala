package me
package modlang
package tfi

package Bindings:
  trait Lang[T] extends Empty.Lang[T]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      toOuter(inner.let(name, toInner(value), v => toInner(in(toOuter(v)))))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      (left.let(name, value._1, v => ((v,v))._1),
      right.let(name, value._2, v => ((v,v))._2))

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      s"let $name = $value :in ${in(name)}"

  class ToString extends ToStringMixin, EvalId[String]
  given ToString()

  trait EvalMixin[T] extends Lang[T]:
    type Expr = () => T
    def let(name: String, value: Expr, in: Expr => Expr): Expr =
      () => in(value)()
