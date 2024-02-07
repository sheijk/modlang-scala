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
    def toOuterRef(r: Ref[inner.Expr]): Ref[Expr]

    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      toOuter(inner.mut(name, toInner(value), v => toInner(in(toOuterRef(v)))))

  trait Dup[T, L <: Lang[T]] extends Lang[T], Empty.Dup[T, L]:
    def fromLeftRef(r: Ref[left.Expr]): Ref[Expr]
    def fromRightRef(r: Ref[right.Expr]): Ref[Expr]

    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      (left.mut(name, value._1, v => in(fromLeftRef(v))._1),
      right.mut(name, value._2, v => in(fromRightRef(v))._2))

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

