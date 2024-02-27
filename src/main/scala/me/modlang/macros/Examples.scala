package me
package modlang
package macro_compiler

def examples: List[(SymEx, Value, tfi.Location)] =
  import SymEx.*
  def greet(n: String) = l(name(n), hello)
  def p(s: String) =
    SymEx.parse(s) match
    case Left(e) => throw Error(e.toString)
    case Right(ex) => ex
  import tfi.CaptureLocation.f
  List(
    f(p("hello"),
     "hello!"),
    f(p("seq hello hello hello"),
     List("hello!", "hello!", "hello!")),
    f(p("seq hello shhht hello"),
     List("hello!")),
    f(p("seq (name foo) hello hello (name bar) hello shhht hello hello"),
     List("hello foo!", "hello foo!", "hello bar!")),
    f(seq(janMode, helloJan, hello),
     List("hello Jan!", "hello!")),
    f(seq(seq(janMode, helloJan), helloJan),
     List("hello Jan!", "error: Unknown id helloJan in helloJan")),

    f(seq(
      p("defmacro (hifoo) ((name foo) hello)"),
      l("hifoo")),
     List("hello foo!")),
    f(seq(
      p("defmacro (hifoo) ((name foo) hello)"),
      sym("hifoo")),
     List("hello foo!")),

    f(seq(
      p("defmacro (swap $left $right) $right $left"),
      l("swap", greet("2nd"), greet("1st"))),
     List("hello 1st!", "hello 2nd!")),
    f(seq(
      p("defmacro (swap $left $right) $right $left"),
      l("swap")),
    List("error: Expected (swap $left $right) in ((swap))")),
    f(seq(
      p("defmacro (swap $left $right) $right $left"),
      l("swap", "too", "many", "args")),
     List("error: Expected (swap $left $right) in ((swap too many args))")),

    // Pattern macros
    f(seq(
      p("(defmacro (foo 1 2 $three) $three)"),
      p(s"(foo 1 2 ${l(name("3rd"), hello)})")),
      List("hello 3rd!")),
    f(seq(
      p("(defmacro (twice (name $name)) ((name $name) hello hello))"),
      p("(twice (name foobar))")),
      List("hello foobar!", "hello foobar!")),
   )

def demo() =
  println("Macro compiler")
  def helloL = HelloLanguage()
  examples.foreach((ex, _, _) => helloL.runAndPrint(ex))
