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
    assertEquals(sym("foo").bindIdsInPattern(l("a", "b")), Left(List("failed in foo")))
    assertEquals(
      l("a", "+", "b").bindIdsInPattern(l("$lhs", "+", "$rhs")),
        Right(List(("$lhs", sym("a")), ("$rhs", sym("b")))))
    assertEquals(
      l("a", "+", l("sum", "b", "c")).bindIdsInPattern(l("$lhs", "+", "$rhs")),
        Right(List(("$lhs", sym("a")), ("$rhs", l("sum", "b", "c")))))
    assertEquals(l("opt", l("+", "0", "5")).bindIdsInPattern(addMulPattern),
      Right(List(("$rhs", sym("5")))))
    assertEquals(l("opt", l("+", "0", l("3", "*", "7"))).bindIdsInPattern(addMulPattern),
      Right(List(("$rhs", l("3", "*", "7")))))
    assertEquals(
      l("foo", "1", "2", l(name("3rd"), hello)).bindIdsInPattern(l("foo", "1", "2", "$three")),
      Right(List(("$three", l(name("3rd"), hello)))))

  test("Parser utils should work"):
    import ParseUtils.*
    def parsed = Right(())
    def tNotToEnd[T](f: Prop[T], s: String) =
      f(ParseState(s))
    def t[T](f: Prop[T], s: String) =
      val state = ParseState(s)
      val r = f(state)
      r.flatMap(ex =>
        if inBounds(state) then
          err(s"Didn't parse to end of input, still at pos ${state.idx}")(state)
        else
          Right(ex))

    assertEquals(t(chr('x'), "x"), parsed)
    assertNotEquals(t(chr('x'), "y"), parsed)
    assertNotEquals(t(chr('x'), "xx"), parsed)

    assertEquals(t(endOfInput, ""), parsed)
    assertNotEquals(t(endOfInput, "x"), parsed)

    assertEquals(t((chr('a') |> chr('b')).ignore, "ab"), parsed)
    assertNotEquals(t((chr('a') |> chr('b')).ignore, "ax"), parsed)

    assertEquals(t(endOfInput, ""), parsed)
    assertEquals(t(endOfInput | chr('x'), ""), parsed)
    assertEquals(t(chr('x') | endOfInput, ""), parsed)
    assertNotEquals(t(endOfInput, "x"), parsed)

    val notId = ws | endOfInput | (chr('(') | chr(')')).ignore
    assertEquals(t(notId, ""), parsed)
    assertEquals(t(notId, " "), parsed)
    assertEquals(t(notId, "("), parsed)
    assertEquals(t(notId, ")"), parsed)
    assertNotEquals(t(notId, "_"), parsed)

    assertEquals(t(substringUntil(endOfInput), "foo, bar"), Right("foo, bar"))
    assertEquals(tNotToEnd(substringUntil(chr(',')), "foo, bar"), Right("foo"))
    assertNotEquals(tNotToEnd(substringUntil(chr(',')).ignore, "foo bar"), parsed)
    assertEquals(tNotToEnd(substringUntil(notId) <* chr(' ') <* chr('!').ignore, "foo !"), Right("foo"))

    assertEquals(tNotToEnd(id, "foo "), Right("foo"))
    assertEquals(t(id, "foo"), Right("foo"))

    import SymEx.*
    import SymEx.ParseRules.{atom, unparenthized}
    assertEquals(t(atom, "foo"), Right(sym("foo")))
    assertEquals(t(atom, "foo "), Right(sym("foo")))
    assertEquals(t(unparenthized, "foo "), Right(sym("foo")))
    assertEquals(t(unparenthized, "foo bar"), Right(l("foo", "bar")))

  test("SymEx.parse should work"):
    val p = SymEx.parse
    import SymEx.*
    assertEquals(p("foo"), Right(sym("foo")))
    assertEquals(p("(foo)"), Right(l("foo")))
    assertEquals(p("((foo))"), Right(l(l("foo"))))
    assertEquals(p("foo bar"), Right(l("foo", "bar")))
    assertEquals(p("(foo bar)"), Right(l("foo", "bar")))
    assertEquals(p("((foo bar))"), Right(l(l("foo", "bar"))))
    assertEquals(p("a b c"), Right(l("a", "b", "c")))
    assertEquals(p("f (a)"), Right(l("f", l("a"))))
    assertEquals(p("f ((a))"), Right(l("f", l(l("a")))))
    assertEquals(p("foo (a b)"), Right(l("foo", l("a", "b"))))
    assertEquals(p("(a b)"), Right(l("a", "b")))
    assertEquals(p("(a b c)"), Right(l("a", "b", "c")))
    assertEquals(p("(defun (foo x y) (ret 0))"),
      Right(l("defun", l("foo", "x", "y"), l("ret", "0"))))
