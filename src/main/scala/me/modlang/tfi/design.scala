package me
package modlang
package tfi

package NewDesign:
  trait Lang[E[T]]

  trait AddL[E[T]] extends Lang[E]:
    def int[Env](v: Int): E[Int]
    def add[Env](lhs: E[Int], rhs: E[Int]): E[Int]
    def mul[Env](lhs: E[Int], rhs: E[Int]): E[Int]

  trait NegL[E[T]] extends Lang[E]:
    def neg[Env](e: E[Int]): E[Int]

  trait LambdaL[E[T]] extends Lang[E]:
    def lambda[I, O](body: E[I] => E[O]): E[I => O]
    def call[R, T](f: E[T => R], x: E[T]): E[R]

  trait FuncL[E[T]] extends AddL[E], NegL[E], LambdaL[E]

  type StringE[T] = String

  case class Show() extends FuncL[StringE]:
    type E[T] = StringE[T]
    override def int[Env](v: Int): E[Int] = v.toString
    override def add[Env](lhs: E[Int], rhs: E[Int]): E[Int] = s"($lhs + $rhs)"
    override def mul[Env](lhs: E[Int], rhs: E[Int]): E[Int] = s"($lhs * $rhs)"
    override def neg[Env](e: E[Int]): E[Int] = s"-($e)"
    override def lambda[I, O](body: E[I] => E[O]): E[I => O] =
      val bd = body("v")
      s"(v => $bd)"
    override def call[R, T](f: E[T => R], x: E[T]): E[R] =
      s"($f $x)"

  type Run[T] = T

  case class Eval() extends FuncL[Run]:
    type E[T] = Run[T]
    override def int[Env](v: Int): E[Int] = v
    override def add[Env](lhs: E[Int], rhs: E[Int]): E[Int] = lhs + rhs
    override def mul[Env](lhs: E[Int], rhs: E[Int]): E[Int] = lhs * rhs
    override def neg[Env](e: E[Int]): E[Int] = -e
    override def lambda[I, O](body: E[I] => E[O]): E[I => O] = x => body(x)
    override def call[R, T](f: E[T => R], x: E[T]): E[R] = f(x)

  def demo() =
    println("NewDesign")
    def a() =
      def test[E[_]](l: FuncL[E]): E[Int] = l.add(l.int(2), l.int(3))
      println(s"  eval(${test(Show())}) = ${test(Eval())}")
    def b() =
      def test[E[_]](l: FuncL[E]) = l.call(l.lambda(v => l.add(l.int(2), v)), l.int(5))
      println(s"  eval(${test(Show())}) = ${test(Eval())}")
    def c() =
      def test[E[_]](l: FuncL[E]) =
        val foo = l.lambda((a: E[Int]) => l.lambda((b: E[Int]) => l.mul(a, b)))
        l.call(l.call(foo, l.int(10)), l.int(5))
      println(s"  eval(${test(Show())}) = ${test(Eval())}")
    a()
    b()
    c()
