package me
package modlang
package expression_problem

package union_model:
  trait Base:
    type Expr >: Lit | Add <: Matchable
    case class Lit(value: Int)
    case class Add(lhs: Expr, rhs: Expr)

    def show(e: Expr): String =
      e match
        case l : Lit => l.value.toString()
        case a : Add => s"${show(a.lhs)} + ${show(a.rhs)}"
        case _ => throw Error("sad lack of type safety")

  trait Neg extends Base:
    type Expr >: Lit | Add | Neg <: Matchable
    case class Neg(e: Expr)

    override def show(e: Expr): String =
      e match
        case n: Neg => s"-${show(n.e)}"
        case _ => super.show(e)

  trait Eval extends Base:
    def eval(e: Expr): Int =
      e match
        case l : Lit => l.value
        case a : Add => eval(a.lhs) + eval(a.rhs)
        case _ => throw Error("sad lack of type safety")

  trait EvalNeg extends Eval, Neg:
    override def eval(e: Expr): Int =
      e match
        case n: Neg => -eval(n.e)
        case _ => super.eval(e)

  def test() =
    class L extends EvalNeg
    val l = L()
    import l.*
    val e = Add(Lit(10), Neg(Lit(5)))
    println(s"  eval(${show(e)}) => ${eval(e)} [union]")

