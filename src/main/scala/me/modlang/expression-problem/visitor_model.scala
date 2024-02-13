package me
package modlang
package expression_problem

// Extended version of visitor from paper. The visitor is now generic so we can
// type safely return values from it's operations
package visitor_model:
  trait Expr[V[T] <: Visitor[T]]:
    def accept[T](v: V[T]): T

  trait Visitor[Type]:
    def visitLit(value: Int): Type
    def visitAdd(lhs: Type, rhs: Type): Type

  class Show extends Visitor[String]:
    def visitLit(value: Int): String = value.toString()
    def visitAdd(lhs: String, rhs: String): String = s"$lhs + $rhs"

  case class Lit[V[T] <: Visitor[T]](value: Int) extends Expr[V]:
    def accept[T](v: V[T]): T = v.visitLit(value)

  case class Add[V[T] <: Visitor[T]](lhs: Expr[V], rhs: Expr[V]) extends Expr[V]:
    def accept[T](v: V[T]): T =
      v.visitAdd(lhs.accept(v), rhs.accept(v))

  // Add neg case
  trait VisitorN[T] extends Visitor[T]:
    def visitNeg(e: T): T

  class ShowN extends Show, VisitorN[String]:
    def visitNeg(e: String): String = "-" + e

  case class Neg[V[T] <: VisitorN[T]](e: Expr[V]) extends Expr[V]:
    def accept[T](v: V[T]): T =
      v.visitNeg(e.accept(v))

  // Add eval operation
  class Eval extends Visitor[Int]:
    def visitLit(value: Int): Int = value
    def visitAdd(lhs: Int, rhs: Int): Int = lhs + rhs

  // Both extensions
  class EvalN extends Eval, VisitorN[Int]:
    def visitNeg(e: Int): Int = -e

  def test() =
    val e = Add(Lit(10), Neg(Lit(5)))
    println(s"  eval(${e.accept(ShowN())}) => ${e.accept(EvalN())} [visitor]")

