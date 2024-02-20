package me
package modlang
package tc

package Dummy:
  trait Lang[T] extends Empty.Lang[T]:
    def dummy(msg: String, e: Expr): Expr

  trait ToStringMixin extends Empty.Lang[String]:
    def dummy(msg: String, e: Expr) = s"<msg $msg $e>"

  trait EvalMixin[T] extends Lang[T], Calc.EvalMixin[T]:
    def dummy(msg: String, e: Expr) = e
