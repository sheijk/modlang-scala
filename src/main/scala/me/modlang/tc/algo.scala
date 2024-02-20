package me
package modlang
package tc

package Algo:
  trait Lang[T] extends Empty.Lang[T]:
    type Loop
    def if_(cond: Expr, onTrue: Expr, onFalse: Expr): Expr
    def loop(name: String, body: Loop => Expr): Expr
    def break(loop: Loop, ret: Expr): Expr

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

  trait EvalMixin[T] extends Lang[T], EvalHasBool[T]:
    type Expr = () => T
    def if_(cond: () => T, onTrue: Expr, onFalse: Expr): Expr =
      () => if asBool(cond) then onTrue() else onFalse()
    type Loop = String
    def loop(name: String, body: String => Expr): Expr =
      () =>
        try
          while true do
            val _ = body(name)()
          throw Error("Quit loop")
        catch case e: LoopBreak[T] =>
          e._2
    def break(loop: Loop, ret: Expr): Expr =
      () =>
        throw LoopBreak[T](loop, ret())
