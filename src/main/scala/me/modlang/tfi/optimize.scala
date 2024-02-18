package me
package modlang
package tfi

import scala.collection.mutable.ListBuffer

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

package Bindings:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]

package Algo_calc_bindings:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T],
    Algo_calc.ConstantFoldMixin[T, Inner],
    Bindings.ConstantFoldMixin[T, Inner]

package References:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]

package Blocks:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]:
    def isConstant(e: Expr): Boolean

    override def block(statements: Expr*): Expr =
      val newStatements = ListBuffer[inner.Expr]()
      statements.foreach(stm =>
        if !isConstant(stm) then
          newStatements.addOne(toInner(stm)))
      if newStatements.isEmpty && !statements.isEmpty then
        newStatements.addOne(toInner(statements.last))
      if newStatements.size == 1 then
        toOuter(newStatements.last)
      else
        toOuter(inner.block(newStatements.toList*))

package Imperative:
  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T],
    References.ConstantFoldMixin[T, Inner],
    Blocks.ConstantFoldMixin[T, Inner],
    Algo_calc_bindings.ConstantFoldMixin[T, Inner]

package Optimizer:
  trait Lang[T] extends Imperative.Lang[T], Dummy.Lang[T]
  type Value = Imperative.Value

  class ToString extends Lang[String], Imperative.ToStringMixin, Dummy.ToStringMixin
  trait EvalMixin extends Lang[Value], Imperative.EvalMixin[Value], Dummy.EvalMixin[Value], EvalFn[Value], EvalFnIntBool[Value]
  class Eval extends EvalMixin

  transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends
      Lang[T],
      Imperative.ConstantFoldMixin[T, Inner]:
    trait Expr:
      def toInner(): inner.Expr
      def toConstantInt(): Option[Int] = None
      def toConstantBool(): Option[Boolean] = None
      def isConstant(): Boolean = false

    case class Dynamic(innerExpr: inner.Expr) extends Expr:
      def toInner(): inner.Expr = innerExpr

    case class ConstantInt(value: Int) extends Expr:
      def toInner(): inner.Expr = inner.int(value)
      override def toConstantInt(): Option[Int] = Some(value)
      override def isConstant(): Boolean = true

    case class ConstantBool(value: Boolean) extends Expr:
      def toInner(): inner.Expr = inner.bool(value)
      override def toConstantBool(): Option[Boolean] = Some(value)
      override def isConstant(): Boolean = true

    def toOuter(e: inner.Expr): Expr = Dynamic(e)
    def toInner(e: Expr): inner.Expr = e.toInner()

    override def int(v: Int): Expr = ConstantInt(v)
    override def bool(v: Boolean): Expr = ConstantBool(v)
    override def dummy(msg: String, e: Expr): Expr =
      toOuter(inner.dummy(msg, toInner(e)))

    inline def toOption[T <: Expr](x: Expr): Option[T] =
      x match
      case t: T => Some(t)
      case _ => None

    def toConstantInt(e: Expr): Option[Int] = e.toConstantInt()
    def toConstantBool(e: Expr): Option[Boolean] = e.toConstantBool()
    def isConstant(e: Expr): Boolean = e.isConstant()

  case class ConstantFold[T, Inner <: Lang[T]](inner_ : Inner) extends
    Empty.Nested[T, Inner](inner_),
    ConstantFoldMixin[T, Inner]:
    type Loop = inner.Loop

  def combineLangStrings(lhs: String, rhs: String): String = s"${lhs} => ${rhs}"
  case class ToStringCombine[L <: Lang[String]](from: L, to: L) extends
      Lang[String],
      Empty.Dup[String, Lang[String]](from, to, combineLangStrings),
      Imperative.Dup[String, Lang[String]],
      Dummy.Dup[String, Lang[String]]

  type Ast = [T] => (l: Lang[T]) => l.Expr

  object syntax2:
    def int(value: Int): Ast = [T] => (l: Lang[T]) => l.int(value)
    def bool(value: Boolean): Ast = [T] => (l: Lang[T]) => l.bool(value)
    def dummy(msg: String, e: Ast): Ast = [T] => (l: Lang[T]) => l.dummy(msg, e(l))

    type Expr = Ast
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
      [T] => (l: Lang[T]) => l.if_(cond(l), onTrue(l), onFalse(l))
    // def loop(name: String, body: Loop => Expr): Expr =
    //   [T] => (l: Lang[T]) => l.loop(name, lp => body(l)(lp))
    // def break(loop: Loop, ret: Expr): Expr
    def block(statements: Expr*): Expr =
      [T] => (l: Lang[T]) =>
        def app(f: Ast) = f(l)
        l.block(statements.map(app)*)
    def mut(name: String, value: Expr, in: [Ref] => Ref => Expr): Expr =
      [T] => (l: Lang[T]) => l.mut(name, value(l), r => in(r)(l))

    extension (lhs: Ast)
      def +(rhs: Ast): Ast = [T] => (l: Lang[T]) => l.plus(lhs(l), rhs(l))
      def &(rhs: Ast): Ast = [T] => (l: Lang[T]) => l.and(lhs(l), rhs(l))
      def >(rhs: Ast): Ast = [T] => (l: Lang[T]) => l.greaterThan(lhs(l), rhs(l))

  def testcases =
    import CaptureLocation.f
    // Calc.testcases ++
    import syntax2.*
    List(
        f(int(15), int(5) + int(10)),
        f(bool(true), bool(true) & bool(true)),
        f(bool(false), int(5) > int(10)),
        f( int(17), int(5) + int(10) + int(2)),
        f( bool(true), int(5) + int(10) > int(5)),
        f( bool(true), int(5) + int(10) > int(5) & int(3) + int(4) > int(2) + int(3)),
        // Dummy won't get optimized
        f( int(12) + dummy("foobar", int(579)),
int(2) + int(10) + dummy("foobar", int(123) + int(456))),
        f( int(10), if_(bool(true), int(10), dummy("", int(100)))),
        f( int(3),
            block(int(1), int(2), int(3))),
        f( mut("foo", int(100), [T] => (foo: T) => int(123)),
           mut("foo", int(100), [T] => (foo: T) => int(123))),
        // f(
        //   mut("foo", int(100), foo => foo.get()),
        //   
        //     mut("foo", int(100), foo =>
        //       block(int(1), int(2), foo.get()))),
    )

  def opt[T](langs: (Lang[T], Lang[String])): (Lang[T], Lang[String]) =
    (ConstantFold(langs._1), ConstantFold(langs._2))

  def runTestLoc[Value, Lang[_] <: Empty.Lang[?]](
    t: ([T] => (l: Lang[T]) => l.Expr, [T] => (l: Lang[T]) => l.Expr, Location),
    eNoOpt: Lang[Value],
    sNoOpt: Lang[String],
    e: Lang[Value],
    s: Lang[String],
  ): Unit =
    val expected = t._1
    val program = t._2
    val location = t._3
    val expectedValue = e.eval(expected(e))
    val source = sNoOpt.eval(program(sNoOpt))
    val sourceOpt = s.eval(program(s))
    val result = e.eval(program(e))
    println(s"  Running $source optimized to $sourceOpt produced $result")
    if result != expectedValue then
      val expectedStr = s.eval(expected(s))
      println(s"$location: error: expected $expectedStr but found $result")

  def demo(): Unit =
    println("Optimizer")
    val l = opt(Eval(), ToString())
    val e : Lang[Value] = l._1
    val s : Lang[String] = l._2
    testcases.foreach(runTestLoc[Value, Lang](_, Eval(), ToString(), e, s))

