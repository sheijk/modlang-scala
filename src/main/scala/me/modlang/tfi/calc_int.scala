package me
package modlang
package tfi

package Calc_int:
  trait Lang[T] extends Empty.Lang[T]:
    def int(v: Int): Expr
    def plus(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def int(v: Int): String = v.toString()
    def plus(lhs: String, rhs: String): String = s"($lhs + $rhs)"

  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T]:
    override def int(v: Int): Expr = fromInt(v)
    override def plus(lhs: Expr, rhs: Expr): Expr = fromInt(asInt(lhs) + asInt(rhs))

    def fromInt(v: Int): Expr
    def asInt(t: Expr): Int

  type Value = Int

  class Eval extends EvalMixin[Value], EvalId[Value], EvalInt
  given Eval()

  def tests() =
    List(
      (10, [T] => (l: Lang[T]) => l.int(10)),
      (20, [T] => (l: Lang[T]) => l.plus(l.int(5), l.int(15))),
    )

  def testing() = tests().foreach(runTest[Value, Lang])

