package me
package modlang
package tfi

// consider adding more types, like TypedExpr[T], etc.

object Calc_bool:
  trait Lang[Expr]:
    def bool(v: Boolean): Expr
    extension (e:Expr)
      def &(rhs: Expr): Expr

  given Lang[String] with
    def bool(v:Boolean):String = v.toString()
    extension (e:String)
      def &(rhs: String): String = s"($e & $rhs)"

  given Lang[Boolean] with
    def bool(v:Boolean):Boolean = v
    extension (e:Boolean)
      def &(rhs: Boolean): Boolean = e & rhs

  object Lang:
    def apply[T](using l : Lang[T]) = l

  def tests[L : Lang] =
    List(
      (true, Lang[L].bool(true)),
      (false, Lang[L].bool(false)),
    )

  def runTest[L : Lang](t : (Boolean, L)) = ""


def foo[L : Calc_bool.Lang](l:L) : L = l & Calc_bool.Lang[L].bool(true)

def bar[L : Calc_bool.Lang]() : L =
  Calc_bool.Lang[L].bool(true) & Calc_bool.Lang[L].bool(true)

def demo() =
  println("Running tfi demo")
  println(foo("xxxx"))
  println(foo(true))
  println(foo(false))
  println(bar[String]())
  println(bar[Boolean]())
  // Calc_bool.tests[Boolean].foreach Calc_bool.runTest
  // ((expected, expr) => if expected != expr then throw Error("blah"))

object Calc_int:
  trait Lang:
    type Value
    type Expr <: Value
    def int(v: Int): Value
    def +(lhs: Expr, rhs:Expr):Expr

  class ToString extends Lang:
    type Value = String
    type Expr = String

    override def int(v: Int): Value = v.toString()
    override def +(lhs: Expr, rhs: Expr): Expr = s"($lhs + $rhs)"

// object Calc:
//   trait Lang extends Calc_bool.Lang, Calc_int.Lang:
//     def <(lhs:Expr, rhs:Expr):Expr

