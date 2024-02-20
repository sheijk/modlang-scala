package me
package modlang
package tc

package References:
  trait Ref[Expr]:
    def set(v: Expr): Expr
    def get(): Expr

  trait Lang[T] extends Empty.Lang[T]:
    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr

  class StringRef(name: String) extends Ref[String]:
    def set(v: String): String = s"($name = $v)"
    def get(): String = name

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def mut(name: String, value: Expr, in: Ref[Expr] => Expr): Expr =
      s"(mut $name = $value :in ${in(new StringRef(name))})"
  given ToStringMixin with EvalId[String] with {}

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

