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

  case class Add[L, R](lhs: L, rhs: R)
  given showAdd[L, R](using sl: Show[L])(using sr: Show[R]) : Show[Add[L, R]] with
    def show(t: Add[L, R]) = s"(${sl.show(t.lhs)} + ${sr.show(t.rhs)})"

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

  given evalAdd[L, R](using el: Eval[Int, L])(using er : Eval[Int, R]) : Eval[Int, Add[L, R]] with
    def eval(t: Add[L, R]): Int = el.eval(t.lhs) + er.eval(t.rhs)

  def run[T](e: T)(using eval : Eval[Int, T])(using show: Show[T]) =
    val src = show.show(e)
    val r = eval.eval(e)
    println(s"  $src => $r")

  def demo() =
    run(Lit(10))
    run(Add(Lit(1), Lit(2)))

package neg:
  import base.{*, given}

  case class Neg[E](e: E)

  given showNeg[E](using s: Show[E]) : Show[Neg[E]] with
    def show(e: Neg[E]): String = "-" + s.show(e.e)

  def demo() =
    printSource(Neg(Lit(10)))
    printSource(Neg(Add(Neg(Lit(1)), Lit(2))))

package both:
  import base.{*, given}
  import neg.{*, given}
  import eval.{*, given}

  given evalNeg[E](using ev: Eval[Int, E]) : Eval[Int, Neg[E]] with
    def eval(e: Neg[E]): Int = -ev.eval(e.e)

  def demo() =
    run(Neg(Lit(10)))
    run(Neg(Add(Neg(Lit(1)), Lit(2))))

package opt:
  import base.{*, given}
  import neg.{*, given}
  import eval.{*, given}

  trait Wrap[T, W[_]]:
    type Out
    def wrap(t: T): W[Out]
  def wrap[T, W[_]](t: T)(using w : Wrap[T, W]): W[w.Out] = w.wrap(t)

  trait Wrap1[T, W[_]]:
    def wrap1(t: T): W[T]

  given optimLit(using ev: Eval[Int, Lit[Int]]) : Wrap[Lit[Int], Expr] with
    type Out = Lit[Int]
    def wrap(ex: Lit[Int]): Expr[Lit[Int]] = Expr(ex, ev)

  given optimAdd[L, R]
      (using ev: Eval[Int, Add[Expr[L], Expr[R]]])
      (using evl: Eval[Int, L])
      (using evr: Eval[Int, R])
      : Wrap[Add[L, R], Expr] with
    type Out = Add[Expr[L], Expr[R]]
    def wrap(e: Add[L, R]): Expr[Add[Expr[L], Expr[R]]] =
      Expr(Add(Expr(e.lhs, evl), Expr(e.rhs, evr)), ev)

  case class Expr[E](ex: E, ev : Eval[Int, E]):
    def eval(): Int = ev.eval(ex)
    override def toString(): String = "e" + ex.toString()

  given evalExpr[E](using ev: Eval[Int, E]) : Eval[Int, Expr[E]] with
    def eval(e: Expr[E]): Int = e.eval()
    override def toString(): String = "???"

  def demo() =
    println("  optimize")
    def opt[E](e: E)(using Wrap[E, Expr]): Unit =
      val ex = wrap(e)
      val r = ex.eval()
      println(s"  opt($e) = $ex => $r")
    opt(Lit(10))
    opt(Add(Lit(1), Lit(2)))
    // opt(Neg(Lit(10)))
    // opt(Neg(Add(Neg(Lit(1)), Lit(2))))

def demo() =
  println("Type classes")
  base.demo()
  eval.demo()
  neg.demo()
  both.demo()
  opt.demo()

