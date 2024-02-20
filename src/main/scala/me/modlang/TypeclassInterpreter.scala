package me
package modlang
package typeclass

trait Expr[Value]:
  def show(): String = this.toString()
  def eval(): Value

trait Show:
  def show(e: Expr[?]): String = e.show()

trait Eval[Value]:
  def eval(e: Expr[Value]): Value = e.eval()

def run(e: Expr[Int]) =
  val src = e.show()
  val r = e.eval()
  println(s"  $src => $r")

case class Lit[Value](value: Value) extends Expr[Value]:
  def eval() = value

trait BinOp[Value](f: (Value, Value) => Value)(lhs: Expr[Value], rhs: Expr[Value]) extends Expr[Value]:
  def eval() = f(lhs.eval(), rhs.eval())

case class Add(lhs: Expr[Int], rhs: Expr[Int]) extends BinOp[Int]((x: Int, y: Int) => x + y)(lhs, rhs)

def baseDemo() =
  run(Add(Lit(1), Lit(2)))

case class Neg(e: Expr[Int]) extends Expr[Int]:
  def eval(): Int = -e.eval()

def newTypeDemo() =
  run(Add(Neg(Lit(10)), Lit(5)))

def demo() =
  println("Type classes")
  baseDemo()
  newTypeDemo()

