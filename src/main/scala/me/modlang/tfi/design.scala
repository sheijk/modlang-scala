package me
package modlang
package tfi

package NewDesign:
  trait Lang[E[T, Env]]

  trait AddL[E[T, Env]] extends Lang[E]:
    def int[Env](v: Int): E[Int, Env]
    def add[Env](lhs: E[Int, Env], rhs: E[Int, Env]): E[Int, Env]

  trait NegL[E[T, Env]] extends Lang[E]:
    def neg[Env](e: E[Int, Env]): E[Int, Env]

  sealed trait Var[T, Env]:
    def toInt: Int
  case class VarZ[T, Env]() extends Var[T, Env]:
    def toInt = 0
  case class VarS[T, Env](n: Var[T, Env]) extends Var[T, (T, Env)]:
    def toInt = 1 + n.toInt

  trait LambdaL[E[T, Env]] extends Lang[E]:
    def v[T, Env](): E[(Env, T), T]
    def lambda[I, O, Env](body: E[I, (I, Env)] => E[O, (I, Env)]): E[I => O, Env]
    def call[R, T, Env](f: E[T => R, Env], x: E[T, Env]): E[R, Env]

  trait FuncL[E[T, Env]] extends AddL[E], NegL[E], LambdaL[E]

  type StringE[T, Env] = String

  case class Show() extends FuncL[StringE]:
    type E[T, Env] = StringE[T, Env]
    override def int[Env](v: Int): E[Int, Env] = v.toString
    override def add[Env](lhs: E[Int, Env], rhs: E[Int, Env]): E[Int, Env] = s"($lhs + $rhs)"
    override def neg[Env](e: E[Int, Env]): E[Int, Env] = s"-($e)"
    override def v[T, Env](): E[(Env, T), T] = s"v"
    override def lambda[I, O, Env](body: E[I, (I, Env)] => E[O, (I, Env)]): E[I => O, Env] =
      val bd = body("v")
      s"(v => $bd)"
    override def call[R, T, Env](f: E[T => R, Env], x: E[T, Env]): E[R, Env] =
      s"$f$x"

  type Run[T, Env] = T

  case class Eval() extends FuncL[Run]:
    type E[T, Env] = Run[T, Env]
    override def int[Env](v: Int): E[Int, Env] = v
    override def add[Env](lhs: E[Int, Env], rhs: E[Int, Env]): E[Int, Env] = lhs + rhs
    override def neg[Env](e: E[Int, Env]): E[Int, Env] = -e
    override def v[T, Env](): E[(Env, T), T] = ???
    override def lambda[I, O, Env](body: E[I, (I, Env)] => E[O, (I, Env)]): E[I => O, Env] = x => body(x)
    override def call[R, T, Env](f: E[T => R, Env], x: E[T, Env]): E[R, Env] = f(x)

  def demo() =
    println("NewDesign")
    // def test[E[_, _]](l: FuncL[E]): E[Int, Any] = l.add(l.int(2), l.int(3))
    def test[E[_, _]](l: FuncL[E]) = l.call(l.lambda(v => l.add(l.int(2), v)), l.int(5))
    def src: String = test(Show())
    def result = test(Eval())
    println(s"  eval($src) = $result")

