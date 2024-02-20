package me
package modlang
package typeclass

package base:
  trait Show[T]:
    type E = T
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

def demo() =
  println("Type classes")
  base.demo()

