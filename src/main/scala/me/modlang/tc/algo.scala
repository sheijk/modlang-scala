package me
package modlang
package tc

package Algo:
  trait Lang[T] extends Empty.Lang[T]:
    type Loop
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr
    def loop(name: String, body: Loop => Expr): Expr
    def break(loop: Loop, ret: Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    type Loop = inner.Loop

    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
      toOuter(inner.if_(toInner(cond), toInner(onTrue), toInner(onFalse)))
    def loop(name: String, body: Loop => Expr): Expr =
      toOuter(inner.loop(name, (l: inner.Loop) => toInner(body(l))))
    def break(loop: Loop, ret: Expr): Expr =
      toOuter(inner.break(loop, toInner(ret)))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    type Loop = (left.Loop, right.Loop)
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
      (left.if_(cond._1, onTrue._1, onFalse._1), right.if_(cond._2, onTrue._2, onFalse._2))
    def loop(name: String, body: Loop => Expr): Expr =
      var result: Expr | Null = null
      left.loop(name, leftL =>
        right.loop(name, rightL =>
          result = body((leftL, rightL))
          result.asInstanceOf[Expr]._2
          )
        result.asInstanceOf[Expr]._1)
      result.asInstanceOf[Expr]
    def break(loop: Loop, ret: Expr): Expr =
      (left.break(loop._1, ret._1),
      right.break(loop._2, ret._2))

  trait ToStringMixin extends Lang[String]:
    type Expr = Result
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr =
      s"(if $cond :then $onTrue :else $onFalse)"
    type Loop = String
    def loop(name: String, body: Loop => Expr): Expr =
      s"(loop $name ${body(name)})"

    def break(loop: Loop, ret: Expr): Expr =
      s"(break $loop :with $ret)"
  given ToStringMixin with EvalId[String] with {}

  case class LoopBreak[Value](name: String, value: Value) extends Exception

  trait EvalMixin[T] extends Lang[T], EvalHasBool[T], EvalFn[T]:
    def if_(cond: () => T, onTrue: Expr, onFalse: Expr): Expr =
      () => if asBool(cond()) then onTrue() else onFalse()
    type Loop = String
    def loop(name: String, body: String => Expr): Expr =
      () =>
        try
          while true do
            val _ = body(name)()
          throw Error("Quit loop")
        catch case e: LoopBreak[T] =>
          assert(e.name == name)
          e._2
    def break(loop: Loop, ret: Expr): Expr =
      () =>
        throw LoopBreak[T](loop, ret())
