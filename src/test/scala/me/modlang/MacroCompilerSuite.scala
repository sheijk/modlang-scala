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
      assertEquals[Any, Any](result, expected, "examples")(using toMunit(loc), munit.Compare.defaultCompare)
    examples.foreach(check)

  test("SymEx.bindIdsInPattern should work"):
    import SymEx.*
    val addMulPattern = l("opt", l("+", "0", "$rhs"))
    assertEquals(sym("foo").bindIdsInPattern(l("a", "b")), List())
    assertEquals(
      l("a", "+", "b").bindIdsInPattern(l("$lhs", "+", "$rhs")),
        List(("$lhs", sym("a")), ("$rhs", sym("b"))))
    assertEquals(
      l("a", "+", l("sum", "b", "c")).bindIdsInPattern(l("$lhs", "+", "$rhs")),
        List(("$lhs", sym("a")), ("$rhs", l("sum", "b", "c"))))
    assertEquals(l("opt", l("+", "0", "5")).bindIdsInPattern(addMulPattern),
      List(("$rhs", sym("5"))))
    assertEquals(l("opt", l("+", "0", l("3", "*", "7"))).bindIdsInPattern(addMulPattern),
      List(("$rhs", l("3", "*", "7"))))
    assertEquals(
      l("foo", "1", "2", l(name("3rd"), hello)).bindIdsInPattern(l("foo", "1", "2", "$three")),
      List(("$three", l(name("3rd"), hello))))

