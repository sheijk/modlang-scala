// package me
// package modlang
// package tc
// 
// import scala.collection.mutable.ListBuffer
// 
// package Calc_int:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Nested[T, Inner]:
//     def toConstantInt(e: Expr): Option[Int]
// 
//     override def plus(lhs: Expr, rhs: Expr): Expr =
//       (toConstantInt(lhs), toConstantInt(rhs)) match
//       case (Some(lhsStatic), Some(rhsStatic)) => int(lhsStatic + rhsStatic)
//       case _ => toOuter(inner.plus(toInner(lhs), toInner(rhs)))
// 
// package Calc_bool:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Nested[T, Inner]:
//     def toConstantBool(e: Expr): Option[Boolean]
// 
//     override def and(lhs: Expr, rhs: Expr): Expr =
//       (toConstantBool(lhs), toConstantBool(rhs)) match
//       case (Some(lhsStatic), Some(rhsStatic)) => bool(lhsStatic & rhsStatic)
//       case _ => toOuter(inner.and(toInner(lhs), toInner(rhs)))
// 
// package Calc:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Calc_int.ConstantFoldMixin[T, Inner], Calc_bool.ConstantFoldMixin[T, Inner], Nested[T, Inner]:
//     override def greaterThan(lhs: Expr, rhs: Expr): Expr =
//       (toConstantInt(lhs), toConstantInt(rhs)) match
//       case (Some(lhsStatic), Some(rhsStatic)) => bool(lhsStatic > rhsStatic)
//       case (l, r) => toOuter(inner.greaterThan(toInner(lhs), toInner(rhs)))
// 
// package Algo:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]:
//     def toConstantBool(e: Expr): Option[Boolean]
// 
//     override def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
//       toConstantBool(cond) match
//         case Some(true) => onTrue
//         case Some(false) => onFalse
//         case None => toOuter(inner.if_(toInner(cond), toInner(onTrue), toInner(onFalse)))
// 
// package Algo_calc:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Algo.ConstantFoldMixin[T, Inner], Calc.ConstantFoldMixin[T, Inner]
// 
// package Bindings:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]
// 
// package Algo_calc_bindings:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T],
//     Algo_calc.ConstantFoldMixin[T, Inner],
//     Bindings.ConstantFoldMixin[T, Inner]
// 
// package References:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]
// 
// package Blocks:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T], Nested[T, Inner]:
//     def isConstant(e: Expr): Boolean
// 
//     override def block(statements: Expr*): Expr =
//       val newStatements = ListBuffer[inner.Expr]()
//       statements.foreach(stm =>
//         if !isConstant(stm) then
//           newStatements.addOne(toInner(stm)))
//       if newStatements.isEmpty && !statements.isEmpty then
//         newStatements.addOne(toInner(statements.last))
//       if newStatements.size == 1 then
//         toOuter(newStatements.last)
//       else
//         toOuter(inner.block(newStatements.toList*))
// 
// package Imperative:
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends Lang[T],
//     References.ConstantFoldMixin[T, Inner],
//     Blocks.ConstantFoldMixin[T, Inner],
//     Algo_calc_bindings.ConstantFoldMixin[T, Inner]
// 
// package Optimizer:
//   trait Lang[T] extends Imperative.Lang[T], Dummy.Lang[T]
//   type Value = Imperative.Value
// 
//   class ToString extends Lang[String], Imperative.ToStringMixin, Dummy.ToStringMixin
//   trait EvalMixin extends Lang[Value], Imperative.EvalMixin[Value], Dummy.EvalMixin[Value], EvalFn[Value], EvalFnIntBool[Value]
//   class Eval extends EvalMixin
// 
//   transparent trait ConstantFoldMixin[T, Inner <: Lang[T]] extends
//       Lang[T],
//       Imperative.ConstantFoldMixin[T, Inner]:
//     trait Expr:
//       def toInner(): inner.Expr
//       def toConstantInt(): Option[Int] = None
//       def toConstantBool(): Option[Boolean] = None
//       def isConstant(): Boolean = false
// 
//     case class Dynamic(innerExpr: inner.Expr) extends Expr:
//       def toInner(): inner.Expr = innerExpr
// 
//     case class ConstantInt(value: Int) extends Expr:
//       def toInner(): inner.Expr = inner.int(value)
//       override def toConstantInt(): Option[Int] = Some(value)
//       override def isConstant(): Boolean = true
// 
//     case class ConstantBool(value: Boolean) extends Expr:
//       def toInner(): inner.Expr = inner.bool(value)
//       override def toConstantBool(): Option[Boolean] = Some(value)
//       override def isConstant(): Boolean = true
// 
//     def toOuter(e: inner.Expr): Expr = Dynamic(e)
//     def toInner(e: Expr): inner.Expr = e.toInner()
// 
//     override def int(v: Int): Expr = ConstantInt(v)
//     override def bool(v: Boolean): Expr = ConstantBool(v)
//     override def dummy(msg: String, e: Expr): Expr =
//       toOuter(inner.dummy(msg, toInner(e)))
// 
//     inline def toOption[T <: Expr](x: Expr): Option[T] =
//       x match
//       case t: T => Some(t)
//       case _ => None
// 
//     def toConstantInt(e: Expr): Option[Int] = e.toConstantInt()
//     def toConstantBool(e: Expr): Option[Boolean] = e.toConstantBool()
//     def isConstant(e: Expr): Boolean = e.isConstant()
// 
//   case class ConstantFold[T, Inner <: Lang[T]](inner_ : Inner) extends
//     Empty.Nested[T, Inner](inner_),
//     ConstantFoldMixin[T, Inner]:
//     type Loop = inner.Loop
// 
//   def combineLangStrings(lhs: String, rhs: String): String = s"${lhs} => ${rhs}"
//   case class ToStringCombine[L <: Lang[String]](from: L, to: L) extends
//       Lang[String],
//       Empty.Dup[String, Lang[String]](from, to, combineLangStrings),
//       Imperative.Dup[String, Lang[String]],
//       Dummy.Dup[String, Lang[String]]
// 
//   def testcases =
//     import CaptureLocation.f
//     // Calc.testcases ++
//     List(
//         f([T] => (l: Lang[T]) => l.int(15),
//           [T] => (l: Lang[T]) =>
//               l.plus(l.int(5), l.int(10))),
//         f([T] => (l: Lang[T]) => l.bool(true),
//           [T] => (l: Lang[T]) =>
//               l.and(l.bool(true), l.bool(true))),
//         f([T] => (l: Lang[T]) => l.bool(false),
//           [T] => (l: Lang[T]) =>
//               l.greaterThan(l.int(5), l.int(10))),
//         f([T] => (l: Lang[T]) => l.int(17),
//           [T] => (l: Lang[T]) =>
//               l.plus(l.plus(l.int(5), l.int(10)), l.int(2))),
//         f([T] => (l: Lang[T]) => l.bool(true),
//           [T] => (l: Lang[T]) =>
//               l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5))),
//         f([T] => (l: Lang[T]) => l.bool(true),
//           [T] => (l: Lang[T]) =>
//             l.and(
//               l.greaterThan(l.plus(l.int(5), l.int(10)), l.int(5)),
//               l.greaterThan(l.plus(l.int(3), l.int(4)), l.plus(l.int(2), l.int(3))))),
//         // Dummy won't get optimized
//         f([T] => (l: Lang[T]) => l.plus(l.int(12), l.dummy("foobar", l.int(579))),
//           [T] => (l: Lang[T]) =>
//             l.plus(l.plus(l.int(2), l.int(10)), l.dummy("foobar", l.plus(l.int(123), l.int(456))))),
//         f([T] => (l: Lang[T]) => l.int(10),
//           [T] => (l: Lang[T]) =>
//             l.if_(l.bool(true), l.int(10), l.dummy("", l.int(100)))),
//         f([T] => (l: Lang[T]) => l.int(3),
//           [T] => (l: Lang[T]) =>
//             l.block(l.int(1), l.int(2), l.int(3))),
//         f([T] => (l: Lang[T]) => l.mut("foo", l.int(100), foo => l.int(123)),
//           [T] => (l: Lang[T]) => l.mut("foo", l.int(100), foo => l.int(123))),
//         f([T] => (l: Lang[T]) =>
//           l.mut("foo", l.int(100), foo => foo.get()),
//           [T] => (l: Lang[T]) =>
//             l.mut("foo", l.int(100), foo =>
//               l.block(l.int(1), l.int(2), foo.get()))),
//     )
// 
//   def opt[T](langs: (Lang[T], Lang[String])): (Lang[T], Lang[String]) =
//     (ConstantFold(langs._1), ConstantFold(langs._2))
// 
//   def runTestLoc[Value, Lang[_] <: Empty.Lang[?]](
//     t: ([T] => (l: Lang[T]) => l.Expr, [T] => (l: Lang[T]) => l.Expr, Location),
//     eNoOpt: Lang[Value],
//     sNoOpt: Lang[String],
//     e: Lang[Value],
//     s: Lang[String],
//   ): Unit =
//     val expected = t._1
//     val program = t._2
//     val location = t._3
//     val expectedValue = e.eval(expected(e))
//     val source = sNoOpt.eval(program(sNoOpt))
//     val sourceOpt = s.eval(program(s))
//     val result = e.eval(program(e))
//     println(s"  Running $source optimized to $sourceOpt produced $result")
//     if result != expectedValue then
//       val expectedStr = s.eval(expected(s))
//       println(s"$location: error: expected $expectedStr but found $result")
// 
//   def demo(): Unit =
//     println("Optimizer")
//     val l = opt(Eval(), ToString())
//     val e : Lang[Value] = l._1
//     val s : Lang[String] = l._2
//     testcases.foreach(runTestLoc[Value, Lang](_, Eval(), ToString(), e, s))
// 
// // package Pipeline_proto:
// //   trait Lang[T]:
// //     type Expr
// //     def extract(e: Expr): T
// //     type Result = T
// // 
// //   type Program = [T] => (l: Lang[T]) => l.Expr
// //   def build(p: Program, l: Lang[?]): l.Expr = p(l)
// //   def run[T](p: Program, l: Lang[T]): T = l.extract(build(p, l))
// //   // def transform[L[_] <: Lang[?]](p: Program, f : [T] => (l: Lang[T]) => Lang[T], l: L[?]): l.Expr = ???
// // 
// //   def demo() =
// //     // def semantic(p: Program, l: Lang[?]) = p(l |> desugar |> typecheck)
// //     def compileAndRun(p: Program, vm: StackL, opt :Boolean) =
// //       val source: String = p(showL)
// //       val vm =
// //         if opt then p |> desugar |> typecheck |> highOpt |> lower |> lowOpt
// //         else  p |> desugar |> typecheck |> lower
// //       val eval = vm |> evalVm
// //       val compiled = vm |> compileVm
// //       compiled()
// // 
// //   // type Program[L[_] <: Lang[?]] = [T] => (l: L[T]) => l.Expr
// //   // def build[Lang](p: Program[Lang], l) = p(l)
