package me
package modlang
package macro_compiler

final class MacroCompilerSuite[Expr] extends TestSuite:
  import tfi.Location
  def toMunit(loc: Location) =
    munit.Location(loc.file, loc.line)

  test("examples should work"):
    def helloL = HelloLanguage()
    def check(ex: SymEx, expected: Value, loc: Location) =
      val program = helloL.compile(ex)
      val result: Value = program()
      assertEquals[Any, Any](expected, result, "examples")(using toMunit(loc), munit.Compare.defaultCompare)
    examples.foreach(check)

