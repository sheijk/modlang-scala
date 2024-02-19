package me
package modlang
package expression_problem

package tcext_model:
  import tc_model.{*, given}

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

  trait NestedLang[T](l: Lang[T]):
    def lit(value: Int): T = l.lit(value)
    def add(lhs: T, rhs: T): T = l.add(lhs, rhs)
  trait NestedLangN[T](n: LangN[T]):
    def neg(e: T): T = n.neg(e)
  given combine[T](using l: Lang[T], n: LangN[T]) : LangC[T] = new LangC[T]
    with NestedLang[T](l)
    with NestedLangN[T](n)

  type Ast = [T] => LangC[T] => T

  object syntax2:
    def lit(value: Int): Ast = [T] => (l: LangC[T]) => l.lit(value)
    def neg(e: Ast): Ast = [T] => (l: LangC[T]) => l.neg(e(l))
    extension (lhs: Ast) def +(rhs: Ast): Ast = [T] => (l: LangC[T]) => l.add(lhs(l), rhs(l))

  def test() =
    import syntax2.*
    val ex = neg(lit(-10) + neg(lit(5)))
    def f[T : LangC](f: Ast): T = f(summon[LangC[T]])
    def test(ex: Ast) =
      val negStr: String = pushNeg(fold(f(ex)))
      val result: Int = f(ex)
      println(s"  eval(${f(ex):String}) =opt=> eval(${negStr}) = ${result} [typeclass ext]")
    test(ex)
    test(lit(1) + lit(2) + lit(3))
    List(lit(0), lit(10), lit(5) + lit(6)).foreach(test)

