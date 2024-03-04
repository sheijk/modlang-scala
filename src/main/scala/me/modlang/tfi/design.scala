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

  type StringE[T] = Int => String

  case class Show() extends FuncL[StringE]:
    type E[T] = StringE[T]
    override def int[Env](v: Int): E[Int] = _ => v.toString
    override def add[Env](lhs: E[Int], rhs: E[Int]): E[Int] = c => s"(${lhs(c)} + ${rhs(c)})"
    override def mul[Env](lhs: E[Int], rhs: E[Int]): E[Int] = c => s"(${lhs(c)} * ${rhs(c)})"
    override def neg[Env](e: E[Int]): E[Int] = c => s"-(${e(c)})"
    override def lambda[I, O](body: E[I] => E[O]): E[I => O] =
      c =>
        val bd = body(_ => s"x.$c")
        s"(x.$c => ${bd(c+1)})"
    override def call[R, T](f: E[T => R], x: E[T]): E[R] =
      c => s"(${f(c)} ${x(c)})"
  def show[T](e: (l: FuncL[StringE]) => StringE[T]) = e(Show())(0)

  type Run[T] = T

  case class Eval() extends FuncL[Run]:
    type E[T] = Run[T]
    override def int[Env](v: Int): E[Int] = v
    override def add[Env](lhs: E[Int], rhs: E[Int]): E[Int] = lhs + rhs
    override def mul[Env](lhs: E[Int], rhs: E[Int]): E[Int] = lhs * rhs
    override def neg[Env](e: E[Int]): E[Int] = -e
    override def lambda[I, O](body: E[I] => E[O]): E[I => O] = x => body(x)
    override def call[R, T](f: E[T => R], x: E[T]): E[R] = f(x)
  def eval[T](e: (l: FuncL[Run]) => T) = e(Eval())

  def demo() =
    println("NewDesign")
    def a() =
      def test[E[_]](l: FuncL[E]): E[Int] = l.add(l.int(2), l.int(3))
      println(s"  eval(${show(test)}) = ${eval(test)}")
    def b() =
      def test[E[_]](l: FuncL[E]) = l.call(l.lambda(v => l.add(l.int(2), v)), l.int(5))
      println(s"  eval(${show(test)}) = ${eval(test)}")
    def c() =
      def test[E[_]](l: FuncL[E]) =
        val foo = l.lambda((a: E[Int]) => l.lambda((b: E[Int]) => l.add(a, l.mul(b, a))))
        l.call(l.call(foo, l.int(10)), l.int(5))
      println(s"  eval(${show(test)}) = ${eval(test)}")
    a()
    b()
    c()
