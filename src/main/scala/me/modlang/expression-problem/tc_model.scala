package me
package modlang
package expression_problem

package tc_model:
  trait Lang[T]:
    def lit(value: Int): T
    def add(lhs: T, rhs: T): T

  given showLang: Lang[String] = new Lang[String]:
    def lit(value: Int): String = value.toString()
    def add(lhs: String, rhs: String): String = s"($lhs + $rhs)"

  trait LangN[T]:
    def neg(e: T): T

  given showLangN: LangN[String] = new LangN[String]:
    def neg(e: String): String = s"-$e"

  given evalLang: Lang[Int] = new Lang[Int]:
    type Expr = Int
    def lit(value: Int): Int = value
    def add(lhs: Int, rhs: Int): Int = lhs + rhs

  given evalLangN : LangN[Int] = new LangN[Int]:
    def neg(e: Int): Int = -e

  object syntax:
    def lit[T](value: Int)(using l: Lang[T]): T = l.lit(value)
    def neg[T](e: T)(using l: LangN[T]): T = l.neg(e)
    extension [T](lhs: T) def +(rhs: T)(using l: Lang[T]): T = l.add(lhs, rhs)
  import syntax.*

  enum CtxNeg { case Pos; case Neg }

  given pushDown[T](using l: Lang[T]) : Lang[CtxNeg => T] = new Lang[CtxNeg => T]:
    def lit(value: Int) : CtxNeg => T =
      case CtxNeg.Pos => l.lit(value)
      case CtxNeg.Neg => l.lit(-value)
    def add(lhs: CtxNeg => T, rhs: CtxNeg => T): CtxNeg => T =
      case CtxNeg.Pos => l.add(lhs(CtxNeg.Pos), rhs(CtxNeg.Pos))
      case CtxNeg.Neg => l.add(lhs(CtxNeg.Neg), rhs(CtxNeg.Neg))

  given pushDownN[T : LangN] : LangN[CtxNeg => T] = new LangN[CtxNeg => T]:
    def neg(e: CtxNeg => T): CtxNeg => T =
      case CtxNeg.Pos => e(CtxNeg.Neg)
      case CtxNeg.Neg => e(CtxNeg.Pos)

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

  trait LangAndN[T] extends Lang[T], LangN[T]
  given cf[T](using l: Lang[T], n: LangN[T]): LangAndN[Folded[T]] = new LangAndN[Folded[T]]:
    def lit(value: Int) = Folded.ConstantInt(value)
    def add(lhs: Folded[T], rhs: Folded[T]): Folded[T] =
      (lhs, rhs) match
      case (Folded.ConstantInt(lhsValue), Folded.ConstantInt(rhsValue)) =>
        Folded.ConstantInt(lhsValue + rhsValue)
      case (_, _) =>
        Folded.Dynamic(l.add(lhs.to(l), rhs.to(l)))
    def neg(e: Folded[T]): Folded[T] =
      e.map(n.neg, i => -i)

  def fold(e: Folded[String]): String = e.to(showLang)

  def test() =
    def ex[T : Lang : LangN] = neg(lit(-10) + neg(lit(5)))
    val negStr: String = fold(ex)
    // val negStr: String = pushNeg(ex)
    val result: Int = pushNeg(ex)
    println(s"  eval(${ex[String]}) =opt=> eval(${negStr}) = ${result} [typeclass]")

