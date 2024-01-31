package me
package modlang
package tfi

package Calc:
  trait Lang[T] extends Calc_int.Lang[T], Calc_bool.Lang[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr

  trait ToStringMixin extends Lang[String], Calc_bool.ToStringMixin, Calc_int.ToStringMixin:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = s"($lhs > $rhs)"

  class ToString extends ToStringMixin
  given ToString()

  trait EvalMixin[T] extends Lang[T], Calc_bool.EvalMixin[T], Calc_int.EvalMixin[T]:
    def greaterThan(lhs: Expr, rhs: Expr): Expr = fromBool(asInt(lhs) > asInt(rhs))

  type Value = Int | Boolean

  class Eval extends EvalMixin[Value]:
    type Expr = Boolean | Int
    def fromBool(v: Boolean): Expr = v
    def asBool(t: Expr): Boolean = t.asInstanceOf[Boolean]
    def fromInt(v: Int): Expr = v
    def asInt(t: Expr): Int = t.asInstanceOf[Int]

  given Eval()

  type MyTest = Test[Value, Lang]
  def tests() =
    Calc_int.tests().asInstanceOf[List[MyTest]] ++
      Calc_bool.tests().asInstanceOf[List[MyTest]] ++
      List(
        (
          true,
          [T] =>
            (l: Lang[T]) =>
              l.and(l.greaterThan(l.int(10), l.int(5)), l.greaterThan(l.int(3), l.int(2))),
        )
      )

  def testing() = tests().foreach(runTest[Value, Lang])
  type Program = [T] => (l: Lang[T]) => l.Expr
