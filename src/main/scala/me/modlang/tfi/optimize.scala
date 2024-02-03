package me
package modlang
package tfi

package Empty:
  transparent trait Nested[T, Inner <: Lang[T]](inner_ : Inner) extends Lang[T]:
    val inner = inner_
    def toOuter(e: inner.Expr): Expr
    def toInner(e: Expr): inner.Expr

    override def eval(e: Expr): Result =
      inner.eval(toInner(e))

package Calc_int:
  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    override def int(v: Int): Expr =
      toOuter(inner.int(v))

    override def plus(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.plus(toInner(lhs), toInner(rhs)))

package Calc_bool:
  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    override def and(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.and(toInner(lhs), toInner(rhs)))

    override def bool(v: Boolean): Expr =
      toOuter(inner.bool(v))

package Calc:
  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Calc_int.Nested[T, Inner], Calc_bool.Nested[T, Inner]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

package Optimizer:
  type Lang[T] = Calc.Lang[T]
  type Value = Calc.Value

  transparent trait ConstantFoldIntMixin[T, Inner <: Lang[T]] extends Calc.Nested[T, Inner]
  transparent trait ConstantFoldBoolMixin[T, Inner <: Lang[T]] extends Calc.Nested[T, Inner]

  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends ConstantFoldIntMixin[T, Inner], ConstantFoldBoolMixin[T, Inner]:
    enum Expr:
      case Dynamic(e: inner.Expr)
      case ConstantInt(i: Int)
      case ConstantBool(b: Boolean)

    def toOuter(e: inner.Expr): Expr =
      Expr.Dynamic(e)

    override def int(v: Int): Expr =
      Expr.ConstantInt(v)

    def toInner(e: Expr): inner.Expr =
      e match
         case Expr.Dynamic(innerExpr) => innerExpr
         case Expr.ConstantInt(i) => inner.int(i)
         case Expr.ConstantBool(b) => inner.bool(b)

    def toConstantInt(e: Expr) =
      e match
        case Expr.ConstantInt(i) => Some(i)
        case _ => None

    // No more knowledge about type Expr details

    override def plus(lhs: Expr, rhs: Expr): Expr =
      (toConstantInt(lhs), toConstantInt(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => toOuter(inner.int(lhsStatic + rhsStatic))
      case _ => toOuter(inner.plus(toInner(lhs), toInner(rhs)))

    override def bool(v: Boolean): Expr =
      Expr.ConstantBool(v)

    override def and(lhs: Expr, rhs: Expr): Expr =
      (lhs, rhs) match
      case (Expr.ConstantBool(lhsStatic), Expr.ConstantBool(rhsStatic)) => toOuter(inner.bool(lhsStatic & rhsStatic))
      case _ => toOuter(inner.and(toInner(lhs), toInner(rhs)))

    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (lhs, rhs) match
      case (Expr.ConstantInt(lhsStatic), Expr.ConstantInt(rhsStatic)) => bool(lhsStatic > rhsStatic)
      case _ => toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  case class ConstantFold[T, Inner <: Lang[T]](inner_ : Inner) extends
    Empty.Nested[T, Inner](inner_),
    ConstantFoldMixin[T, Inner]

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
        f(true,
          [T] => (l: Lang[T]) =>
              l.and(l.bool(true), l.bool(true))),
      )

  def opt(langs: (Lang[Value], Lang[String])): (Lang[Value], Lang[String]) =
    (ConstantFold(langs._1), ToStringCombine(langs._2, ConstantFold(langs._2)))

  def demo(): Unit =
    println("Optimizer")
    val l = opt(Calc.Eval(), Calc.ToString())
    given e : Lang[Value] = l._1
    given s : Lang[String] = l._2
    testcases.foreach(runTestLoc[Value, Lang])

