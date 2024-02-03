package me
package modlang
package tfi

package Optimizer:
  type Lang[T] = Calc.Lang[T]
  type Value = Calc.Value

  case class ConstantFoldInt[T, Nested <: Calc.Lang[T]](inner: Nested) extends Calc.Lang[T]:
    type Expr = Either[inner.Expr, Int]

    def toOuter(e: inner.Expr): Expr =
      Left(e)

    def toInner(e: Expr): inner.Expr =
      e match
         case Left(innerExpr) => innerExpr
         case Right(i) => inner.int(i)

    override def int(v: Int): Expr =
      Right(v)

    override def plus(lhs: Expr, rhs: Expr): Expr =
      (lhs, rhs) match
      case (Left(lhsDynamic), Left(rhsDynamic)) => toOuter(inner.plus(lhsDynamic, rhsDynamic))
      case (Left(lhsDynamic), Right(rhsStatic)) => toOuter(inner.plus(lhsDynamic, inner.int(rhsStatic)))
      case (Right(lhsStatic), Left(rhsDynamic)) => toOuter(inner.plus(inner.int(lhsStatic), rhsDynamic))
      case (Right(lhsStatic), Right(rhsStatic)) => toOuter(inner.plus(inner.int(lhsStatic), inner.int(rhsStatic)))

    override def eval(e: Expr): Result =
      inner.eval(toInner(e))

    override def and(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.and(toInner(lhs), toInner(rhs)))

    override def bool(v: Boolean): Expr =
      toOuter(inner.bool(v))

    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  case class ToStringCombine(from: Calc.Lang[String], to: Calc.Lang[String]) extends Calc.Lang[String]:
    type Expr = (from.Expr, to.Expr)
    override def eval(e: Expr): String = s"${from.eval(e._1)} => ${to.eval(e._2)}"
    override def int(v: Int): Expr = (from.int(v), to.int(v))
    override def plus(lhs: Expr, rhs: Expr): Expr =
      (from.plus(lhs._1, rhs._1), to.plus(lhs._2, rhs._2))
    override def and(lhs: Expr, rhs: Expr): Expr =
      (from.and(lhs._1, rhs._1), to.and(lhs._2, rhs._2))
    override def bool(v: Boolean): Expr = (from.bool(v), to.bool(v))
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (from.greaterThan(lhs._1, rhs._1), to.greaterThan(lhs._2, rhs._2))

  def testcases: List[Calc.MyTest] =
    import CaptureLocation.f
    Calc.testcases ++
    List(
        f(15,
          [T] => (l: Calc.Lang[T]) =>
              l.plus(l.int(5), l.int(10))),
        f(true,
          [T] => (l: Calc.Lang[T]) =>
              l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5))),
        f(true,
          [T] => (l: Calc.Lang[T]) =>
            l.and(
              l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5)),
              l.greaterThan(l.plus(l.int(3), l.int(4)), l.plus(l.int(2), l.int(3))))),
      )

  def demo(): Unit =
    println("Optimizer")
    given ToStringCombine(Calc.ToString(), ConstantFoldInt[String, Calc.ToString](Calc.ToString()))
    given ConstantFoldInt[Calc.Value, Calc.Eval](Calc.Eval())
    testcases.foreach(runTestLoc[Calc.Value, Calc.Lang])

