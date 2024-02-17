package me
package modlang
package expression_problem

package tcext_model:
  import tc_model.{*, given}

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

  object Folded:
    def map2[T](lhs: Folded[T], rhs: Folded[T], f: (T, T) => T, fConstant: (Int, Int) => Int)(using l: Lang[T]): Folded[T] =
      (lhs, rhs) match
      case (Folded.ConstantInt(lhsValue), Folded.ConstantInt(rhsValue)) =>
        Folded.ConstantInt(fConstant(lhsValue, rhsValue))
      case (_, _) =>
        Folded.Dynamic(f(lhs.to(l), rhs.to(l)))

  trait LangAndN[T] extends Lang[T], LangN[T]
  given cf[T](using l: Lang[T], n: LangN[T]): LangAndN[Folded[T]] = new LangAndN[Folded[T]]:
    def lit(value: Int) = Folded.ConstantInt(value)
    def add(lhs: Folded[T], rhs: Folded[T]): Folded[T] =
      Folded.map2(lhs, rhs, l.add, (l, r) => l + r)
    def neg(e: Folded[T]): Folded[T] =
      e.map(n.neg, i => -i)

  def fold[T](e: Folded[T])(using l: Lang[T]): T = e.to(l)

  def test() =
    import syntax.*
    def ex[T : Lang : LangN] = neg(lit(-10) + neg(lit(5)))
    val negStr: String = fold(pushNeg(ex))
    val result: Int = fold(pushNeg(ex))
    println(s"  eval(${ex[String]}) =opt=> eval(${negStr}) = ${result} [typeclass ext]")

