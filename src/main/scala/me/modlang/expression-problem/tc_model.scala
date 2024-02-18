package me
package modlang
package expression_problem

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

