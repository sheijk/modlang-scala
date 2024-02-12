package me
package modlang
package extension_problem

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

package function_model:
  enum Expr:
    case Lit(value: Int)
    case Add(lhs: Expr, rhs: Expr)

  def show(e: Expr): String =
   e match
     case Expr.Lit(value) => value.toString()
     case Expr.Add(lhs, rhs) => s"${show(lhs)} + ${show(rhs)}"

  // Adding new operation
  def eval(e: Expr): Int =
   e match
     case Expr.Lit(value) => value
     case Expr.Add(lhs, rhs) => eval(lhs) + eval(rhs)

  // Adding new type, can't nest a neg in other expressions :\
  package wont_work:
    enum ExprN:
      case Base(e: Expr)
      case Neg(e: ExprN)

    def show(e: ExprN): String =
      e match
        case ExprN.Base(e) => function_model.show(e)
        case ExprN.Neg(e) => s"-${show(e)}"

    def eval(e: ExprN): Int =
      e match
        case ExprN.Base(e) => function_model.eval(e)
        case ExprN.Neg(e) => -eval(e)

package visitor_model {} // from paper

// Read source [9] from "The extension problem revisited" by Mads Torgensen and
// add functional approaches here. Solutions for immutable objects only are
// allowed.

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

def demo() =
  println("Extension problem")
  class_model.test()
  tfi_model.test()

