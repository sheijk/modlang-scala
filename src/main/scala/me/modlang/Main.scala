package me
package modlang

def demo[Expr](
)(using
  a: Ast[Expr] & Runner[Expr]
) =
  import a.*
  println(s"Running $name")
  val programs = List(
    plus(c(10), c(5)),
    and(greaterThan(c(10), c(5)), greaterThan(c(3), c(2))),
    and(c(true), c(false)),
    and(greaterThan(c(10), c(5)), greaterThan(c(10), c(5))),
    plus(c(10), c(20)),
  )
  programs.foreach(runAndPrint(_))

def andreMode() =
  import stackvm.*
  println("Modular language proto services started")
  runSource("1  2 5 + +")
  runSource("10 2 + 11 > true &&")
  // repl()

@main def Main(args: String*): Unit =
  println("─" * 100)
  println("hello nix")

  demo()(using AdtAst())
  demo()(using UnionAst())
  andreMode()
  typeclass.demo()
  tfi.demo()
  macro_compiler.demo()

  println("─" * 100)
