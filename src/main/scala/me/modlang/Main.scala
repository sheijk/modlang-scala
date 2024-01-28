package me
package modlang

def demo[Expr](
)(using
  a: Ast[Expr] & Runner[Expr]
) =
  import a.*
  println(s"Running $name")
  val programs = List(
    plus(int(10), int(5)),
    and(greaterThan(int(10), int(5)), greaterThan(int(3), int(2))),
    and(bool(true), bool(false)),
    and(greaterThan(int(10), int(5)), greaterThan(int(10), int(5))),
    plus(int(10), int(20)),
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

  println("─" * 100)
