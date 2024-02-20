package me
package modlang
package typeclass

trait Expr[Value]:
  def show(): String = this.toString()
  def eval[T <: Value](): Value

trait Show:
  def show(e: Expr[?]): String = e.show()

trait Eval[Value]:
  def eval(e: Expr[Value]): Value = e.eval()

case class Lit[Value](value: Value) extends Expr[Value]:
  def eval[T]() = value

trait BinOp[Value](f: (Value, Value) => Value)(lhs: Expr[Value], rhs: Expr[Value]) extends Expr[Value]:
  def eval[T]() = f(lhs.eval(), rhs.eval())

case class Add(lhs: Expr[Int], rhs: Expr[Int]) extends BinOp[Int]((x: Int, y: Int) => x + y)(lhs, rhs)

def baseDemo() =
  println("Type classes")
  def run(e: Expr[Int]) =
    val src = e.show()
    val r = e.eval()
    println(s"  $src => $r")
  run(Add(Lit(1), Lit(2)))

def demo() =
  baseDemo()

