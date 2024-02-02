package me
package modlang
package tfi

package Optimizer:
  case class ConstantFoldInt[T, Nested <: Calc.Lang[T]](inner: Nested) extends Calc.Lang[T]:
    type Expr = Either[inner.Expr, Int]

    extension (e: Either[inner.Expr, Int])
      def toInner =
        e match
        case Left(innerExpr) => innerExpr
        case Right(value) => inner.int(value)

    def mapIntInt(lhs: Expr, rhs: Expr, f: (Int, Int) => Int): Expr =
      lhs.flatMap(lhsValue => rhs.map(rhsValue => f(lhsValue, rhsValue)))

    override def int(v: Int): Expr =
      Right(v)

    override def plus(lhs: Expr, rhs: Expr): Expr =
      mapIntInt(lhs, rhs, (lhs, rhs) => lhs + rhs)

    override def eval(e: Expr): Result =
      val innerExpr = e.match
        case Left(dynamic) => dynamic
        case Right(i) => inner.int(i)
      inner.eval(innerExpr)

    override def and(lhs: Expr, rhs: Expr): Expr =
      Left(inner.and(lhs.toInner, rhs.toInner))

    override def bool(v: Boolean): Expr =
      Left(inner.bool(v))

    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      Left(inner.greaterThan(lhs.toInner, rhs.toInner))

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

