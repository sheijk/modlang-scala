package me
package modlang
package macro_compiler

type Value = Int | Boolean

sealed trait AstType
case class Builtin(name: String) extends AstType
case class Macro(name: String) extends AstType

case class Expr[Head](id: Head, childs: List[Expr[Head]] = List())

case class Language(name: String, builtins: Set[Builtin]):
  override def toString(): String = s"Language($name, ${builtins.size} builtins)"

def language(name: String, builtins: Builtin*): Language =
  Language(name, builtins.toSet)

case class Program[Ex](
  name: String,
  language: Language,
  exprs: List[Ex],
):
  override def toString(): String =
    val body = exprs.map(_.toString).mkString("\n  ")
    s"Program($name), $language,\n  {$body})"

type UntypedProgram = Program[Expr[String]]
type TypedProgram = Program[Expr[AstType]]

extension (p: UntypedProgram)
  def expand(): TypedProgram =
    def expandExpr(e: Expr[String]): Expr[AstType] =
      e match
        case Expr("hello", List()) => Expr(Builtin("hello"), List())
        case _ => ???
    new TypedProgram(p.name, p.language, p.exprs.map(expandExpr))

def run(program: UntypedProgram) =
  val typed = program.expand()
  println(s"$program\n=>\n$typed")

val langHello = language("langHello", Builtin("hello"))

def demo() =
  println("Macro compiler")
  run(new UntypedProgram("foo", langHello, List(Expr[String]("hello"), Expr[String]("hello"))))
