package me
package modlang
package tfi

package Blocks:
  trait Lang[T] extends Empty.Lang[T]:
    def block(statements: Expr*): Expr

  trait ToStringMixin extends Lang[String], EvalId[String]:
    def block(statements: String*): Expr =
      "{" + statements.mkString("; ") + "}"

  class ToString extends ToStringMixin, EvalId[String]
  given ToString()

  trait EvalMixin[T] extends Lang[T], EvalFn[T]:
    def block(statements: Expr*): Expr =
      def run(last: Result, statements: List[Expr]): Result =
        statements match
        case Nil => last
        case next :: rem => run(next(), rem)
      statements.toList match
        case Nil => throw Error("empty blocks not allowed")
        case first :: rem => () => run(first(), rem)

