package me
package modlang
package tfi

package References:
  trait Ref[Expr]:
    def set(v: Expr): Expr
    def get(): Expr

  trait Lang[T] extends Empty.Lang[T]:
    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr

  transparent trait Nested[T, Inner <: Lang[T]] extends Lang[T], Empty.Nested[T, Inner]:
    class NestedRef(innerRef: Ref[inner.Expr]) extends Ref[Expr]:
      def get(): Expr = toOuter(innerRef.get())
      def set(v: Expr) = toOuter(innerRef.set(toInner(v)))

    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      toOuter(inner.mut(name, toInner(value), v => toInner(in(NestedRef(v)))))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    class PairRef(leftRef: Ref[left.Expr], rightRef: Ref[right.Expr]) extends Ref[Expr]:
      def set(v: Expr): Expr = (leftRef.set(v._1), rightRef.set(v._2))
      def get(): Expr = (leftRef.get(), rightRef.get())

    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      var result : Expr | Null = null
      left.mut(name, value._1, leftRef =>
        right.mut(name, value._2, rightRef =>
          result = in(PairRef(leftRef, rightRef))
          result.asInstanceOf[Expr]._2)
        result.asInstanceOf[Expr]._1)
      result.asInstanceOf[Expr]

  class StringRef(name: String) extends Ref[String]:
    def set(v: String): String = s"($name = $v)"
    def get(): String = name

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      s"(mut $name = $value :in ${in(new StringRef(name))})"

  class ToString extends ToStringMixin, EvalId[String]
  given ToString()

  class FnRef[Value](initial: Value) extends Ref[() => Value]:
    type Expr = () => Value
    var value = initial
    def set(v: Expr): Expr =
      () =>
        this.value = v()
        value
    def get(): Expr = () => value

  trait EvalMixin[T] extends Lang[T], EvalFn[T]:
    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      val ref = FnRef[Result](value())
      () => in(ref)()

