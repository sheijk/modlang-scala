package me
package modlang
package expression_problem

package tfi_model:
  trait Lang:
    type Expr
    def lit(value: Int): Expr
    def add(lhs: Expr, rhs: Expr): Expr

  class Show extends Lang:
    type Expr = String
    def lit(value: Int): Expr = value.toString()
    def add(lhs: Expr, rhs: Expr): Expr = s"$lhs + $rhs"

  trait LangN extends Lang:
    def neg(e: Expr): Expr

  class ShowN extends Show, LangN:
    def neg(e: Expr): Expr = s"-$e"

  class Eval extends Lang:
    type Expr = Int
    def lit(value: Int): Expr = value
    def add(lhs: Expr, rhs: Expr): Expr = lhs + rhs

  class EvalN extends Eval, LangN:
    def neg(e: Expr): Expr = -e

  def test() =
    def e(l: LangN): l.Expr = l.add(l.lit(10), l.neg(l.lit(5)))
    println(s"  eval(${e(ShowN())}) => ${e(EvalN())} [tfi]")

