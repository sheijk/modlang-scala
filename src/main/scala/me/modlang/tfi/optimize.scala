package me
package modlang
package tfi

package Optimizer:
  type Lang[T] = Calc.Lang[T]
  type Value = Calc.Value

  trait Nested[T, Inner <: Lang[T]](inner_ : Inner) extends Lang[T]:
    val inner = inner_
    def toOuter(e: inner.Expr): Expr
    def toInner(e: Expr): inner.Expr

    override def eval(e: Expr): Result =
      inner.eval(toInner(e))

    override def int(v: Int): Expr =
      toOuter(inner.int(v))

    override def plus(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.plus(toInner(lhs), toInner(rhs)))

    override def and(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.and(toInner(lhs), toInner(rhs)))

    override def bool(v: Boolean): Expr =
      toOuter(inner.bool(v))

    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  case class ConstantFoldInt[T, Inner <: Lang[T]](inner_ : Inner) extends Nested[T, Inner](inner_):
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
      case (Right(lhsStatic), Right(rhsStatic)) => toOuter(inner.int(lhsStatic + rhsStatic))
      case _ => toOuter(inner.plus(toInner(lhs), toInner(rhs)))

  case class ToStringCombine(from: Lang[String], to: Lang[String]) extends Lang[String]:
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
          [T] => (l: Lang[T]) =>
              l.plus(l.int(5), l.int(10))),
        f(true,
          [T] => (l: Lang[T]) =>
              l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5))),
        f(true,
          [T] => (l: Lang[T]) =>
            l.and(
              l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5)),
              l.greaterThan(l.plus(l.int(3), l.int(4)), l.plus(l.int(2), l.int(3))))),
      )

  def demo(): Unit =
    println("Optimizer")
    given ToStringCombine(Calc.ToString(), ConstantFoldInt[String, Calc.ToString](Calc.ToString()))
    given ConstantFoldInt[Value, Calc.Eval](Calc.Eval())
    testcases.foreach(runTestLoc[Value, Lang])

