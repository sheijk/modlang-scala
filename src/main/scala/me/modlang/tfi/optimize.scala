package me
package modlang
package tfi

package Calc_int:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Nested[T, Inner]:
    def toConstantInt(e: Expr): Option[Int]

    override def plus(lhs: Expr, rhs: Expr): Expr =
      (toConstantInt(lhs), toConstantInt(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => int(lhsStatic + rhsStatic)
      case _ => toOuter(inner.plus(toInner(lhs), toInner(rhs)))

package Calc_bool:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Nested[T, Inner]:
    def toConstantBool(e: Expr): Option[Boolean]

    override def and(lhs: Expr, rhs: Expr): Expr =
      (toConstantBool(lhs), toConstantBool(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => bool(lhsStatic & rhsStatic)
      case _ => toOuter(inner.and(toInner(lhs), toInner(rhs)))

package Calc:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Calc_int.ConstantFoldMixin[T, Inner], Calc_bool.ConstantFoldMixin[T, Inner], Nested[T, Inner]:
    override def greaterThan(lhs: Expr, rhs: Expr): Expr =
      (toConstantInt(lhs), toConstantInt(rhs)) match
      case (Some(lhsStatic), Some(rhsStatic)) => bool(lhsStatic > rhsStatic)
      case (l, r) => toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))

package Algo:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]:
    def toConstantBool(e: Expr): Option[Boolean]

    override def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
      toConstantBool(cond) match
        case Some(true) => onTrue
        case Some(false) => onFalse
        case None => toOuter(inner.if_(toInner(cond), toInner(onTrue), toInner(onFalse)))

package Algo_calc:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Algo.ConstantFoldMixin[T, Inner], Calc.ConstantFoldMixin[T, Inner]

package Optimizer:
  trait Lang[T] extends Algo_calc.Lang[T], Dummy.Lang[T]
  type Value = Algo_calc.Value

  class ToString extends Lang[String], Algo_calc.ToStringMixin, Dummy.ToStringMixin
  trait EvalMixin extends Lang[Value], Algo_calc.EvalMixin[Value], Dummy.EvalMixin[Value], EvalFn[Value], EvalFnIntBool[Value]
  class Eval extends EvalMixin

  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends
      Lang[T],
      Algo_calc.ConstantFoldMixin[T, Inner]:
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
    ConstantFoldMixin[T, Inner]:
    type Loop = inner.Loop

  def combineLangStrings(lhs: String, rhs: String): String = s"${lhs} => ${rhs}"
  case class ToStringCombine[L <: Lang[String]](from: L, to: L) extends
      Lang[String],
      Empty.Dup[String, Lang[String]](from, to, combineLangStrings),
      Algo_calc.Dup[String, Lang[String]],
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
        f([T] => (l: Lang[T]) => l.int(10),
          [T] => (l: Lang[T]) =>
            l.if_(l.bool(true), l.int(10), l.dummy("", l.int(100)))),
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

