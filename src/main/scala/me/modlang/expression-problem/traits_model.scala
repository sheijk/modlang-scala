package me
package modlang
package expression_problem

// Traits based solution based on "Independently Extensible Solutions to the
// Expression Problem" by M. Zenger, M. Odersky
package traits_model:
  trait Base:
    type Expr <: ExprBase
    trait ExprBase:
      def show(): String

    class Lit(value: Int) extends ExprBase:
      def show(): String = value.toString()

    class Add(lhs: Expr, rhs: Expr) extends ExprBase:
      def show(): String = s"${lhs.show()} + ${rhs.show()}"

  trait Neg extends Base:
    class Neg(e: Expr) extends ExprBase:
      def show(): String = s"-${e.show()}"

  trait Eval extends Base:
    type Expr <: ExprEval
    trait ExprEval extends ExprBase:
      def eval(): Int

    class LitE(value: Int) extends Lit(value), ExprEval:
      def eval(): Int = value

    class AddE(lhs: Expr, rhs: Expr) extends Add(lhs, rhs), ExprEval:
      def eval(): Int = lhs.eval() + rhs.eval()

  trait EvalNeg extends Eval, Neg:
    class NegE(e: Expr) extends Neg(e), ExprEval:
      def eval(): Int = -e.eval()

  class L extends EvalNeg:
    type Expr = ExprEval

  def test() =
    val l = L()
    import l.*
    val e = AddE(LitE(10), NegE(LitE(5)))
    println(s"  eval(${e.show()}) => ${e.eval()} [traits]")

