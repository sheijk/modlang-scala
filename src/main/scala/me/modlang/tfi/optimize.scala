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

  transparent trait ConstantFoldIntMixin[T, Inner <: Lang[T]] extends Calc.Nested[T, Inner]:
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

  transparent trait ConstantFoldBoolMixin[T, Inner <: Lang[T]] extends Calc.Nested[T, Inner]:
    type Expr = Either[inner.Expr, Boolean]

    def toOuter(e: inner.Expr): Expr =
      Left(e)

    def toInner(e: Expr): inner.Expr =
      e match
         case Left(innerExpr) => innerExpr
         case Right(i) => inner.bool(i)

    override def bool(v: Boolean): Expr =
      Right(v)

    override def and(lhs: Expr, rhs: Expr): Expr =
      (lhs, rhs) match
      case (Right(lhsStatic), Right(rhsStatic)) => toOuter(inner.bool(lhsStatic & rhsStatic))
      case _ => toOuter(inner.and(toInner(lhs), toInner(rhs)))

    // Doesn't work because we only know about static bool values here
    // Need to implement this with flat composition and abstract over value type, again
    // Maybe offer matching methods toConstantInt(e): Option[Int], etc.
    // Or split into impl for Calc_bool, Calc_int, and Calc using both
    // OCaml version only has one combined optimizer for Calc for exactly this reason
    // Maybe optimizer needs to get an interpreter (it already has one as the inner parameter. but probably needs a Lang[Int|Boolean])
    // Make an abstract interpreter which has type Value = Dynamic(innner.Expr) | Int | Boolean? maybe Dynamic can just be [...]
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (lhs, rhs) match
      case (Right(lhsStatic), Right(rhsStatic)) => toOuter(inner.bool(lhsStatic > rhsStatic))
      case _ => toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  case class ConstantFoldInt[T, Inner <: Lang[T]](inner_ : Inner) extends
    Empty.Nested[T, Inner](inner_),
    ConstantFoldIntMixin[T, Inner]
  case class ConstantFoldBool[T, Inner <: Lang[T]](inner_ : Inner) extends
    Empty.Nested[T, Inner](inner_),
    ConstantFoldBoolMixin[T, Inner]

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

  def foldInt(langs: (Lang[Value], Lang[String])): (Lang[Value], Lang[String]) =
    (ConstantFoldInt(langs._1), ToStringCombine(langs._2, ConstantFoldInt(langs._2)))

  def foldBool(langs: (Lang[Value], Lang[String])): (Lang[Value], Lang[String]) =
    (ConstantFoldBool(langs._1), ToStringCombine(langs._2, ConstantFoldBool(langs._2)))

  def opt(langs: (Lang[Value], Lang[String])): (Lang[Value], Lang[String]) =
    (ConstantFoldInt(ConstantFoldBool(langs._1)), ToStringCombine(langs._2, ConstantFoldInt(ConstantFoldBool(langs._2))))

  def demo(): Unit =
    println("Optimizer")
    val l = opt(Calc.Eval(), Calc.ToString())
    given e : Lang[Value] = l._1
    given s : Lang[String] = l._2
    testcases.foreach(runTestLoc[Value, Lang])

