package me
package modlang
package macro_compiler

package StringL:
  // trait Lang[T] extends Empty.Lang[T]:
  //   def string(v: String): Expr
  //   def list(exs: Expr*): Expr

  trait ParseLang[T, L[T] <: tfi.Empty.Lang[T]](val lang: L[T]):
    type Parser = SymEx => Option[lang.Expr]
    type SymbolTable = Scope[Parser]
    def builtins(): SymbolTable
    def parseString(str: String): Option[lang.Expr]

    def parse(ex: SymEx): Option[lang.Expr] =
      val creators = builtins()
      ex.id().flatMap(id =>
        val create = creators.lookup(id)
        create match
        case Some(f) => f(ex)
        case None => parseString(id))

  trait ParseCalcInt[T] extends ParseLang[T, tfi.Calc_int.Lang]:
    def builtins(): SymbolTable =
     val s = SimpleScope[Parser](None)
     s.register("plus", ex =>
       ex match
       case Tree.Node(List(Tree.Leaf("plus"), lhs, rhs)) =>
         parse(lhs).flatMap(l => parse(rhs).map(r => lang.plus(l, r)))
       case _ => None)
     s

    def parseString(str: String): Option[lang.Expr] =
      str.toIntOption.map(lang.int)

  trait ParseCalcBool[T] extends ParseLang[T, tfi.Calc_bool.Lang]:
    def builtins(): SymbolTable =
     val s = SimpleScope[Parser](None)
     s.register("plus", ex =>
       ex match
       case Tree.Node(List(Tree.Leaf("and"), lhs, rhs)) =>
         parse(lhs).flatMap(l => parse(rhs).map(r => lang.and(l, r)))
       case _ => None)
     s

    def parseString(str: String): Option[lang.Expr] =
      if str == "true" then Some(lang.bool(true))
      else if str == "false" then Some(lang.bool(false))
      else None

  trait ParseCalc[T] extends ParseLang[T, tfi.Calc.Lang], ParseCalcBool[T], ParseCalcInt[T]:
    override def builtins(): SymbolTable =
     val s = SimpleScope[Parser](None)
     s.register("plus", ex =>
       ex match
       case Tree.Node(List(Tree.Leaf("greaterThan"), lhs, rhs)) =>
         parse(lhs).flatMap(l => parse(rhs).map(r => lang.greaterThan(l, r)))
       case _ => None)
     s

    override def parseString(str: String): Option[lang.Expr] =
      super[ParseCalcBool].parseString(str).orElse(super[ParseCalcInt].parseString(str))

  def demo() =
    println("Parsing tfi language")
    import tfi.*
    import tfi.Calc_int.{given}
    import tfi.Calc_bool.{given}
    import tfi.Calc.{given}
    given ParseCalcInt[Int] with ParseLang[Int, Calc_int.Lang](summon[Calc_int.Lang[Int]]) with {}
    given ParseCalcBool[Boolean] with ParseLang[Boolean, Calc_bool.Lang](summon[Calc_bool.Lang[Boolean]]) with {}
// [error] -- Error: /Users/jr/Documents/Development/modular-lang-proto/external/temp/modlang/src/main/scala/me/modlang/macros/StringL.scala:73:10 
// [error] 73 |    given ParseCalc[Calc.Value] with ParseLang[Calc.Value, Calc.Lang](summon[Calc.Lang[Calc.Value]]) with {}
// [error]    |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// [error]    |object given_ParseCalc_Value_ParseLang_Value_Lang cannot be instantiated since it has conflicting base types me.modlang.macro_compiler.StringL.ParseLang[me.modlang.tfi.Calc.Value, me.modlang.tfi.Calc_bool.Lang] &
// [error]    |  me.modlang.macro_compiler.StringL.ParseLang[me.modlang.tfi.Calc.Value, me.modlang.tfi.Calc_int.Lang] and me.modlang.macro_compiler.StringL.ParseLang[me.modlang.tfi.Calc.Value, me.modlang.tfi.Calc.Lang]
    given ParseCalc[Calc.Value] with ParseLang[Calc.Value, Calc.Lang](summon[Calc.Lang[Calc.Value]]) with {}
    run[Int, tfi.Calc_int.Lang]("10", "plus 10 20", "plus (plus 5 5) 20")
    run[Boolean, tfi.Calc_bool.Lang]("true", "false", "and true (or true false)")
    run[Calc.Value, tfi.Calc.Lang]("true", "false", "and true (or true false)")

  def run[T, L[T] <: tfi.Empty.Lang[T]](src: String*)(using eval : L[T], parser : ParseLang[T, L]) =
    def p(src: String) =
      val ex: Option[parser.lang.Expr] = parser.parse(SymEx.parse(src).getOrElse(SymEx.sym("parsing error")))
      ex match
      case Some(ex) =>
        println(s"  eval(${src}) = ${parser.lang.eval(ex)}")
      case None =>
        println(s"failed to parse $src")
    src.foreach(p)
