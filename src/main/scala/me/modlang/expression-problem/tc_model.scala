package me
package modlang
package expression_problem

// Approach following this translation of Oleg's paper
// https://gist.github.com/OlivierBlanvillain/48bb5c66dbb0557da50465809564ee80
package tc_model:
  trait Lang[T]:
    def lit(value: Int): T
    def add(lhs: T, rhs: T): T

  trait ShowLang extends Lang[String]:
    def lit(value: Int): String = value.toString()
    def add(lhs: String, rhs: String): String = s"($lhs + $rhs)"
  given showLang: ShowLang with {}

  trait LangN[T]:
    def neg(e: T): T

  trait ShowLangN extends LangN[String]:
    def neg(e: String): String = s"-$e"
  given showLangN: ShowLangN with {}

  trait EvalLang extends Lang[Int]:
    type Expr = Int
    def lit(value: Int): Int = value
    def add(lhs: Int, rhs: Int): Int = lhs + rhs
  given evalLang: EvalLang with {}

  trait EvalLangN extends LangN[Int]:
    def neg(e: Int): Int = -e
  given evalLangN : EvalLangN with {}

  object syntax:
    def lit[T](value: Int)(using l: Lang[T]): T = l.lit(value)
    def neg[T](e: T)(using l: LangN[T]): T = l.neg(e)
    extension [T](lhs: T) def +(rhs: T)(using l: Lang[T]): T = l.add(lhs, rhs)

  def test() =
    import syntax.*
    def ex[T : Lang : LangN] = neg(lit(-10) + neg(lit(5)))
    println(s"  eval(${ex[String]}) = ${ex[Int]} [typeclass]")

  import scala.quoted.*
  import tcext_model.{Ast, LangC}
  def ast10Impl()(using Quotes) : Expr[Ast] = '{[T] => (l: LangC[T]) => l.lit(10)}
  inline def ast10(): Ast = ${ ast10Impl() }

