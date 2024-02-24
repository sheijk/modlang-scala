package me
package modlang
package macro_compiler

def examples: List[(SymEx, Value, tfi.Location)] =
  import SymEx.*
  def greet(n: String) = l(name(n), hello)
  import tfi.CaptureLocation.f
  List(
    f(sym("hello"),
     "hello!"),
    f(seq("hello", "hello", "hello"),
     List("hello!", "hello!", "hello!")),
    f(seq("hello", "shhht", "hello"),
     List("hello!")),
    f(seq(name("foo"), hello, hello, name("bar"), hello, shhht, hello, hello),
     List("hello foo!", "hello foo!", "hello bar!")),
    f(seq(janMode, helloJan, hello),
     List("hello Jan!", "hello!")),
    f(seq(seq(janMode, helloJan), helloJan),
     List("hello Jan!", "error: Unknown id helloJan in helloJan")),

    f(seq(
      l("defmacro", l("hifoo"), l(name("foo"), hello)),
      l("hifoo")),
     List("hello foo!")),
    f(seq(
      l("defmacro", l("hifoo"), l(name("foo"), hello)),
      sym("hifoo")),
     List("hello foo!")),

    f(seq(
      l("defmacro", l("swap", "$left", "$right"), "$right", "$left"),
      l("swap", greet("2nd"), greet("1st"))),
     List("hello 1st!", "hello 2nd!")),
    // f(l("defmacro", l("invalidMacro", "invalidName"), "nope"),
    //   "error: Macro parameter should be param starting with a $ in (invalidName)"),
    // f(seq(
    //   l("defmacro", l("swap", "$left", "$right"), "$right", "$left"),
    //   l("swap")),
    // List("error: Expected (swap $left $right) but found 0 arguments in ((swap))")),
    // f(seq(
    //   l("defmacro", l("swap", "$left", "$right"), "$right", "$left"),
    //   l("swap", "too", "many", "args")),
    //  List("error: Expected (swap $left $right) but found 3 arguments in ((swap too many args))")),

    // Pattern macros
    f(seq(
      l("defmacro", l("foo", "1", "2", "$three"), sym("$three")),
      l("foo", "1", "2", l(name("3rd"), hello))),
      List("hello 3rd!")),
    f(seq(
      l("defmacro", l("twice", l("name", "$name")), l(name("$name"), "hello", "hello")),
      l("twice", name("foobar"))),
      List("hello foobar!", "hello foobar!")),
    // f(seq(l("defmacro", l("defun1", l("$name", "$arg"), "$body")
   )

def demo() =
  println("Macro compiler")
  def helloL = HelloLanguage()
  examples.foreach((ex, _, _) => helloL.runAndPrint(ex))
