package me
package modlang
package typeclass

package base:
  trait Show[T]:
    def show(e: T): String

  def show[T: Show](t: T): String = summon[Show[T]].show(t)
  def printSource[T : Show](t: T) =
    val src: String = show(t)
    println(s"  source($t) = $src")

  case class Lit[Value](value: Value)

  given showLit[T] : Show[Lit[T]] with
    def show(t: Lit[T]) = t.value.toString()

  case class Add[E](lhs: E, rhs: E)
  given showAdd[E](using s: Show[E]) : Show[Add[E]] with
    def show(t: Add[E]) = s"${s.show(t.lhs)} + ${s.show(t.rhs)}"

  def demo() =
    printSource(Lit(10))
    printSource(Add(Lit(1), Lit(2)))

package eval:
  import base.{*, given}

  trait Eval[Value, E]:
    def eval(e: E): Value

  given evalLit[T] : Eval[T, Lit[T]] with
    type Value = T
    def eval(t: Lit[T]): T = t.value

  given evalAdd[E](using e: Eval[Int, E]) : Eval[Int, Add[E]] with
    def eval(t: Add[E]): Int = e.eval(t.lhs) + e.eval(t.rhs)

  def run[T](e: T)(using eval : Eval[Int, T])(using show: Show[T]) =
    val src = show.show(e)
    val r = eval.eval(e)
    println(s"  $src => $r")

  def demo() =
    run(Lit(10))
    run(Add(Lit(1), Lit(2)))

def demo() =
  println("Type classes")
  base.demo()
  eval.demo()

