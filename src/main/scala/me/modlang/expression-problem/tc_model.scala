package me
package modlang
package expression_problem

package tc_model:
  trait Lang[T]:
    def lit(value: Int): T
    def add(lhs: T, rhs: T): T

  given showLang: Lang[String] = new Lang[String]:
    def lit(value: Int): String = value.toString()
    def add(lhs: String, rhs: String): String = s"$lhs + $rhs"

  trait LangN[T]:
    def neg(e: T): T

  implicit val showLangN: LangN[String] = new LangN[String]:
    def neg(e: String): String = s"-$e"

  given evalLang: Lang[Int] = new Lang[Int]:
    type Expr = Int
    def lit(value: Int): Int = value
    def add(lhs: Int, rhs: Int): Int = lhs + rhs

  given evalLangN : LangN[Int] = new LangN[Int]:
    def neg(e: Int): Int = -e

  object syntax:
    def lit[T](value: Int)(using l: Lang[T]): T = l.lit(value)
    def neg[T](e: T)(using l: LangN[T]): T = l.neg(e)
    extension [T](lhs: T) def +(rhs: T)(using l: Lang[T]): T = l.add(lhs, rhs)
  import syntax.*

  def test() =
    def e[T : Lang : LangN]: T = lit(10) + neg(lit(5))
    // def e[T : Lang : LangN]: T = lit(10) + lit(neg(5))
    // ^- not sure why neg(5) is legal here. Looks like evalLang typeclass is
    // pulled in and used here
    println(s"  eval(${e[String]}) => ${e[Int]} [typeclass]")

