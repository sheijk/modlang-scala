package me
package modlang
package expression_problem

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

