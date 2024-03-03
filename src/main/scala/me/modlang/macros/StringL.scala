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

  def demo() =
    println("Parsing tfi language")
    import tfi.Calc_int.{*, given}
    given ParseCalcInt[Int] with ParseLang[Int, Lang](summon[Lang[Int]]) with {}
    run[tfi.Calc_int.Lang]("10", "plus 10 20", "plus (plus 5 5) 20")

  def run[L[Int] <: tfi.Empty.Lang[Int]](src: String*)(using eval : L[Int], parser : ParseLang[Int, L]) =
    def p(src: String) =
      val ex: Option[parser.lang.Expr] = parser.parse(SymEx.parse(src).getOrElse(SymEx.sym("parsing error")))
      ex match
      case Some(ex) =>
        println(s"  eval(${src}) = ${parser.lang.eval(ex)}")
      case None =>
        println(s"failed to parse $src")
    src.foreach(p)
