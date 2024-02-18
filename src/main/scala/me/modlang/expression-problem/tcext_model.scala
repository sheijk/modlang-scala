package me
package modlang
package expression_problem

package tcext_model:
  import tc_model.*
  import scala.quoted.*

  enum CtxNeg { case Pos; case Neg }

  trait PushDown[T](l: Lang[T]) extends Lang[CtxNeg => T]:
    def lit(value: Int) : CtxNeg => T =
      case CtxNeg.Pos => l.lit(value)
      case CtxNeg.Neg => l.lit(-value)
    def add(lhs: CtxNeg => T, rhs: CtxNeg => T): CtxNeg => T =
      case CtxNeg.Pos => l.add(lhs(CtxNeg.Pos), rhs(CtxNeg.Pos))
      case CtxNeg.Neg => l.add(lhs(CtxNeg.Neg), rhs(CtxNeg.Neg))
  given pushDown[T](using l: Lang[T]) : PushDown[T](l) with {}

  trait PushDownN[T] extends LangN[CtxNeg => T]:
    def neg(e: CtxNeg => T): CtxNeg => T =
      case CtxNeg.Pos => e(CtxNeg.Neg)
      case CtxNeg.Neg => e(CtxNeg.Pos)
  given pushDownN[T : LangN] : PushDownN[T] with {}

  trait LangC[T] extends Lang[T], LangN[T]
  trait PushDownLangC[T] extends LangC[CtxNeg => T], PushDown[T], PushDownN[T]
  given pushDownLangC[T : LangC](using l: LangC[T]) : PushDown[T](l) with PushDownN[T] with {}

  def pushNeg[T](e: CtxNeg => T): T = e(CtxNeg.Pos)

  enum Folded[T]:
    case Dynamic(t: T)
    case ConstantInt(value: Int)
    def to(l: Lang[T]): T =
      this match
        case Dynamic(t) => t
        case ConstantInt(value) => l.lit(value)
    def map(fT: T => T, fInt: Int => Int): Folded[T] =
      this match
        case Dynamic(t) => Dynamic(fT(t))
        case ConstantInt(value) => ConstantInt(fInt(value))

  object Folded:
    def map2[T](
        lhs: Folded[T],
        rhs: Folded[T],
        f: (T, T) => T,
        fConstant: (Int, Int) => Int)
        (using l: Lang[T])
        : Folded[T] =
      (lhs, rhs) match
      case (Folded.ConstantInt(lhsValue), Folded.ConstantInt(rhsValue)) =>
        Folded.ConstantInt(fConstant(lhsValue, rhsValue))
      case (_, _) =>
        Folded.Dynamic(f(lhs.to(l), rhs.to(l)))

  trait ConstantFold[T](l: Lang[T], n: LangN[T]) extends LangC[Folded[T]]:
    def lit(value: Int) = Folded.ConstantInt(value)
    def add(lhs: Folded[T], rhs: Folded[T]): Folded[T] =
      Folded.map2(lhs, rhs, l.add, (l, r) => l + r)(using l)
    def neg(e: Folded[T]): Folded[T] =
      e.map(n.neg, i => -i)
  given cf[T](using l: Lang[T], n: LangN[T]): ConstantFold[T](l, n) with {}

  def fold[T](e: Folded[T])(using l: Lang[T]): T = e.to(l)

  given evalC : LangC[Int] with EvalLang with EvalLangN with {}
  given showC : LangC[String] with ShowLang with ShowLangN with {}

  // given evalF(using l: LangC[Int]) : LangC[[T] => (l: LangC[T]) => T] = ???
  // given showF(using l: LangC[String]) : LangC[(l: LangC[String]) => String] = ???
  case class Wrap[T](t: T)
  type Ast = [T] => LangC[T] => T

  // def convert(x: Expr[Any])(using Quotes): Expr[Any] = '{[T] => (l: Lang[T]) => l.lit(10)}
  def ast(x: Expr[Any])(using Quotes): Expr[Any] = '{
    def tmp[T: LangC] = $x;
    val ex = [T] => (l: LangC[T]) => tmp(using l);
    ex
  }

  // def ast10() : Ast = [T] => (l: LangC[T]) => l.lit(10)

  // def ast10Impl()(using Quotes) : Expr[Ast] = '{[T] => (l: LangC[T]) => l.lit(10)}
  // inline def ast10(): Ast = ${ ast10Impl() }

  // object syntax2:
  //   def lit[T](value: Int)(using l: Lang[T]): T = l.lit(value)
  //   def neg[T](e: T)(using l: LangN[T]): T = l.neg(e)
  //   extension [T](lhs: T) def +(rhs: T)(using l: Lang[T]): T = l.add(lhs, rhs)

  def test() =
    import syntax.*
    def exF[T : LangC] = neg(lit(-10) + neg(lit(5)))
    // val ex : Ast = [T] => (l: LangC[T]) => exF(using l)
    val ex = ast10()
    // def exAsPolyMethod[T](l: LangC[T]): T = ex(l)
    // def f[T: LangC](f: LangC[T] => T) = f(summon[LangC[T]])
    def f[T : LangC](f: [T2] => (LangC[T2]) => T2): T = f(summon[LangC[T]])
    val negStr: String = pushNeg(fold(f(ex)))
    val result: Int = f(ex)
    println(s"  eval(${f(ex):String}) =opt=> eval(${negStr}) = ${result} [typeclass ext]")

