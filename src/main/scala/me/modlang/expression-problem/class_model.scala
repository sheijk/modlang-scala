package me
package modlang
package expression_problem

package class_model:
  trait Expr:
    def show(): String
  class Add(lhs: Expr, rhs: Expr) extends Expr:
    def show() = s"${lhs.show()} + ${rhs.show()}"
  class Lit(value: Int) extends Expr:
    def show() = value.toString()

  // Adding a new data case is easy
  case class Neg(e: Expr) extends Expr:
    def show() = s"-${e.show()}"

  // Adding a new operation not so much
  trait ExprE extends Expr:
    def eval(): Int
  class AddE(lhs: ExprE, rhs: ExprE) extends Add(lhs, rhs), ExprE:
    def eval() = lhs.eval() + rhs.eval()
  class LitE(value: Int) extends Lit(value), ExprE:
    def eval() = value

  // Fill intersection of new type and function
  class NegE(e: ExprE) extends Neg(e), ExprE:
    def eval() = -e.eval()

  def test() =
    val e = AddE(LitE(10), NegE(LitE(5)))
    println(s"  eval(${e.show()}) => ${e.eval()} [class]")

