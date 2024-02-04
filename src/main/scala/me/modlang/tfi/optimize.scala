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

  trait Dup[T, L <: Lang[T]](val left : L, val right : L, mergeLangs: (T, T) => T) extends Lang[T]:
    type Expr = (left.Expr, right.Expr)
    override def eval(e: Expr) =
      mergeLangs(left.eval(e._1), right.eval(e._2))

package Dummy:
  trait Lang[T] extends Empty.Lang[T]:
    def dummy(msg: String, e: Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    def dummy(msg: String, e: Expr) = toOuter(inner.dummy(msg, toInner(e)))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    def dummy(msg: String, e: Expr) = (left.dummy(msg, e._1), right.dummy(msg, e._2))

  trait ToStringMixin extends Empty.Lang[String]:
    def dummy(msg: String, e: Expr) = s"<msg $msg $e>"

package Calc_int:
  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    override def int(v: Int): Expr =
      toOuter(inner.int(v))

    override def plus(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.plus(toInner(lhs), toInner(rhs)))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    override def int(v: Int): Expr =
      (left.int(v), right.int(v))

    override def plus(lhs: Expr, rhs: Expr): Expr =
      (left.plus(lhs._1, rhs._1), right.plus(lhs._2, rhs._2))

package Calc_bool:
  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    override def and(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.and(toInner(lhs), toInner(rhs)))

    override def bool(v: Boolean): Expr =
      toOuter(inner.bool(v))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    override def and(lhs: Expr, rhs: Expr): Expr =
      (left.and(lhs._1, rhs._1), right.and(lhs._2, rhs._2))
    override def bool(v: Boolean): Expr =
      (left.bool(v), right.bool(v))

package Calc:
  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Calc_int.Nested[T, Inner], Calc_bool.Nested[T, Inner]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Calc_int.Dup[T, L], Calc_bool.Dup[T, L]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (left.greaterThan(lhs._1, rhs._1), right.greaterThan(lhs._2, rhs._2))

package Optimizer:
  trait Lang[T] extends Calc.Lang[T], Dummy.Lang[T]
  type Value = Calc.Value | String

  class ToString extends Lang[String], Calc.ToStringMixin, Dummy.ToStringMixin
  class Eval extends Lang[Value], Calc.EvalMixin[Value], EvalId[Value], EvalIntBool[Value]:
    def dummy(msg: String, e: Expr) = e

  transparent trait ConstantFoldIntMixin[T, Inner <: Lang[T]] extends Calc_int.Nested[T, Inner]:
    def toConstantInt(e: Expr): Option[Int]

    override def plus(lhs: Expr, rhs: Expr): Expr =
      (toConstantInt(lhs), toConstantInt(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => int(lhsStatic + rhsStatic)
      case _ => toOuter(inner.plus(toInner(lhs), toInner(rhs)))

  transparent trait ConstantFoldBoolMixin[T, Inner <: Lang[T]] extends Calc_bool.Nested[T, Inner]:
    def toConstantBool(e: Expr): Option[Boolean]

    override def and(lhs: Expr, rhs: Expr): Expr =
      (toConstantBool(lhs), toConstantBool(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => bool(lhsStatic & rhsStatic)
      case _ => toOuter(inner.and(toInner(lhs), toInner(rhs)))

  transparent trait ConstantFoldCalcMixin[T, Inner <: Lang[T]] extends Lang[T], ConstantFoldIntMixin[T, Inner], ConstantFoldBoolMixin[T, Inner]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (toConstantInt(lhs), toConstantInt(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => bool(lhsStatic > rhsStatic)
      case (l, r) => toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends ConstantFoldCalcMixin[T, Inner]:
    case class Dynamic(val innerExpr: inner.Expr)
    type Expr = Dynamic | Int | Boolean

    def toOuter(e: inner.Expr): Expr =
      Dynamic(e)

    def toInner(e: Expr): inner.Expr =
      e match
         case e: Dynamic => e.innerExpr
         case i: Int => inner.int(i)
         case b: Boolean => inner.bool(b)

    override def int(v: Int): Expr = v
    override def bool(v: Boolean): Expr = v
    override def dummy(msg: String, e: Expr): Expr =
      toOuter(inner.dummy("opt:" + msg, toInner(e)))

    inline def toOption[T <: Expr](x: Expr): Option[T] =
      x match
      case t: T => Some(t)
      case _ => None

    def toConstantInt(e: Expr): Option[Int] = toOption[Int](e)
    def toConstantBool(e: Expr): Option[Boolean] = toOption[Boolean](e)

  case class ConstantFold[T, Inner <: Lang[T]](inner_ : Inner) extends
    Empty.Nested[T, Inner](inner_),
    ConstantFoldMixin[T, Inner]

  def combineLangStrings(lhs: String, rhs: String): String = s"${lhs} => ${rhs}"
  case class ToStringCombine(from: Lang[String], to: Lang[String]) extends
      Lang[String],
      Empty.Dup[String, Lang[String]](from, to, combineLangStrings),
      Calc.Dup[String, Lang[String]],
      Dummy.Dup[String, Lang[String]]

  def testcases =
    import CaptureLocation.f
    // Calc.testcases ++
    List(
        f([T] => (l: Lang[T]) => l.int(15),
          [T] => (l: Lang[T]) =>
              l.plus(l.int(5), l.int(10))),
        f([T] => (l: Lang[T]) => l.bool(true),
          [T] => (l: Lang[T]) =>
              l.and(l.bool(true), l.bool(true))),
        f([T] => (l: Lang[T]) => l.bool(false),
          [T] => (l: Lang[T]) =>
              l.greaterThan(l.int(5), l.int(10))),
        f([T] => (l: Lang[T]) => l.int(17),
          [T] => (l: Lang[T]) =>
              l.plus(l.plus(l.int(5), l.int(10)), l.int(2))),
        f([T] => (l: Lang[T]) => l.bool(true),
          [T] => (l: Lang[T]) =>
              l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5))),
        f([T] => (l: Lang[T]) => l.bool(true),
          [T] => (l: Lang[T]) =>
            l.and(
              l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5)),
              l.greaterThan(l.plus(l.int(3), l.int(4)), l.plus(l.int(2), l.int(3))))),
        // Dummy won't get optimized
        f([T] => (l: Lang[T]) => l.plus(l.int(12), l.dummy("foobar", l.plus(l.int(123), l.int(456)))),
          [T] => (l: Lang[T]) =>
            l.plus(l.plus(l.int(2), l.int(10)), l.dummy("foobar", l.plus(l.int(123), l.int(456))))),
    )

  def opt[T](langs: (Lang[T], Lang[String])): (Lang[T], Lang[String]) =
    (ConstantFold(langs._1), ToStringCombine(langs._2, ConstantFold(langs._2)))

  def runTestLoc[Value, Lang[_] <: Empty.Lang[?]](
    t: ([T] => (l: Lang[T]) => l.Expr, [T] => (l: Lang[T]) => l.Expr, Location)
  )(using
    s: Lang[String],
    e: Lang[Value],
  ): Unit =
    val expected = t._1
    val program = t._2
    val location = t._3
    val expectedValue = e.eval(expected(e))
    val source = s.eval(program(s))
    val result = e.eval(program(e))
    println(s"Running $source produced $result")
    if result != expectedValue then
      val expectedStr = s.eval(expected(s))
      println(s"$location: error: expected $expectedStr but found $result")

  def demo(): Unit =
    println("Optimizer")
    val l = opt(Eval(), ToString())
    given e : Lang[Value] = l._1
    given s : Lang[String] = l._2
    testcases.foreach(runTestLoc[Value, Lang])

