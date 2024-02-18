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
  // given cf[T](using l: Lang[T], n: LangN[T]): ConstantFold[T](l, n) with {}
  given cfC[T](using l: LangC[T]): ConstantFold[T](l, l) with {}

  def fold[T](e: Folded[T])(using l: Lang[T]): T = e.to(l)

  given evalC : LangC[Int] with EvalLang with EvalLangN with {}
  given showC : LangC[String] with ShowLang with ShowLangN with {}

  // given apply[T](using l: LangC[T]) : LangC[[T2] => LangC[T2] => T2] = ???
  // given apply[T](using l: LangC[[T2] => LangC[T2] => T2])(using li: LangC[T]) : LangC[T] = ???
  // given apply[T](using l: LangC[T]) : LangC[[T2] => LangC[T2] => T2] = new LangC[LangC[T] => T]:
  //   type R = [T2] => LangC[T2] => T2
  //   def lit(value: Int): R = l.lit
  //   def add(lhs: LangC[T] => T, rhs: LangC[T] => T): R = throw Error("")
  //   def neg(e: LangC[T] => T): R = throw Error("")

  // given apply[T](using l: LangC[LangC[T] => T]) : LangC[T] = new LangC[T]:
  //   def lit(value: Int): T = throw Error("")
  //   def add(lhs: T, rhs: T): T = throw Error("")
  //   def neg(e: T): T = throw Error("")
    // def lit(value: Int) = l.lit(value)
    // def add(lhs: T, rhs: T): T = l.add(lhs, rhs)
    // def neg(e: T): T = l.neg(e)

  // case class Expr(f: [T] => (l: LangC[T]) => T, evalL: LangC[Int], showL: LangC[String]):
  //   def eval(): Int = f(evalL)
  //   def show(): String = f(showL)
  trait Expr:
    def eval(): Int
    def show(): String
    def toL[T](newL: LangC[T]): T
    def to(newL: [T] => (l: LangC[T]) => LangC[T]): Expr

  class ExprF(f: [T] => (l: LangC[T]) => T, evalL: LangC[Int], showL: LangC[String]) extends Expr:
    def eval(): Int = f(evalL)
    def show(): String = f(showL)
    def toL[T](newL: LangC[T]): T = f(newL)
    def to(newL: [T] => (l: LangC[T]) => LangC[T]): Expr = ExprF(f, newL(evalL), newL(showL))

  def m(f: [T] => (l: LangC[T]) => T)
      (using evalL: LangC[Int])
      (using showL: LangC[String])
      : Expr = new ExprF(f, evalL, showL)

  def test() =
    import syntax.*
    // type Ast[T, L[T] <: Lang[T], N[T] <: LangN[T]] = T
    def ex[T : LangC] = neg(lit(-10) + neg(lit(5)))
    // def ex2[T](l: LangC[T]): T = l.lit(3)
    // def ex[T](l: LangC[T]): T = l.neg(l.add(l.lit(-10), l.neg(l.lit(5))))
    val ex2 = [T] => (l: LangC[T]) => ex(using l)
    // val ex2 = [T] => (l: LangC[T]) => l.neg(l.add(l.lit(-10), l.neg(l.lit(5))))
    // val ex[T](using LangC[T]) = ex2(l)
    // val expr = m(ex2)
    // def folded2[T](l: LangC[T]): LangC[T] = cfC(using l)
    // val expr = m(ex2).to(folded)
    val expr = m(ex2)
    val negStr = expr.show()
    val result = expr.eval()
    def folded[T](l: LangC[T]): LangC[Folded[T]] = cfC(using l)
    val optStr = expr.toL(folded(showC))
    // val optStr: String = expr.toL(cfC(using showC))
    // val negStr: String = ex2
    // val negStr: String = fold(pushNeg(ex))
    // val result: Int = fold(pushNeg(ex))
    // val negStr: String = ex
    // val result: Int = pushNeg(fold(ex2))
    println(s"  eval(${negStr}) =opt=> eval(${optStr}) = ${result} [typeclass ext]")

