package me
package modlang
package tfi

package Dummy:
  trait Lang[T] extends Empty.Lang[T]:
    def dummy(msg: String, e: Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    def dummy(msg: String, e: Expr) = toOuter(inner.dummy(msg, toInner(e)))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    def dummy(msg: String, e: Expr) = (left.dummy(msg, e._1), right.dummy(msg, e._2))

  trait ToStringMixin extends Empty.Lang[String]:
    def dummy(msg: String, e: Expr) = s"<msg $msg $e>"

  trait EvalMixin[T >: Int | Boolean] extends Lang[T], Calc.EvalMixin[T], EvalId[T], EvalIntBool[T]:
    def dummy(msg: String, e: Expr) = e
