package me
package modlang
package expression_problem

package tcext_model:
  import tc_model.*

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
    def map2[T](lhs: Folded[T], rhs: Folded[T], f: (T, T) => T, fConstant: (Int, Int) => Int)(using l: Lang[T]): Folded[T] =
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

  def test() =
    import syntax.*
    type Ast[T, L[T] <: Lang[T], N[T] <: LangN[T]] = T
    def ex[T : LangC] : Ast[T, Lang, LangN] = neg(lit(-10) + neg(lit(5)))
    val negStr: String = pushNeg(fold(ex))
    // val negStr: String = fold(pushNeg(ex))
    // val result: Int = fold(pushNeg(ex))
    // val negStr: String = ex
    val result: Int = pushNeg(fold(ex))
    println(s"  eval(${ex[String]}) =opt=> eval(${negStr}) = ${result} [typeclass ext]")

