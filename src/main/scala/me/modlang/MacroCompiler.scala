package me
package modlang
package macro_compiler

type Value = Int | Boolean

opaque type BaseExpr = String

sealed trait AstType
trait Builtin(val name: String) extends AstType:
  def expand(e: Symex): Expr[AstType]
  override def toString() = s"Builtin $name"
trait Macro(val name: String) extends AstType

enum Expr[Head]:
  case Atom(hd: Head)
  case List(childs: List[Expr[Head]])
type Symex = Expr[String]

case class Language(name: String, builtins: Map[String, Builtin]):
  def lookup(id: String): Option[AstType] =
    builtins get id
  def expand(e: Symex): Expr[AstType] =
    e match
      case Expr.Atom(hd) =>
        lookup(hd) match
          case Some(builtin : Builtin) => builtin.expand(e)

def language(name: String, builtins: Builtin*): Language =
  Language(name, builtins.map((b: Builtin) => (b.name, b)).toMap)

case class Program[Ex](
  name: String,
  language: Language,
  exprs: List[Ex],
):
  override def toString(): String =
    val body = exprs.map(_.toString).mkString("\n  ")
    s"Program $name :language ${language.name} {\n  $body\n}"

type UntypedProgram = Program[Symex]
type TypedProgram = Program[Expr[AstType]]

extension (p: UntypedProgram)
  def expand(): TypedProgram =
    new TypedProgram(p.name, p.language, p.exprs.map(p.language.expand(_)))

def run(program: UntypedProgram) =
  val typed = program.expand()
  println(s"$program\n=>\n$typed")

val langHello =
  val hello = new Builtin("hello"):
    def expand(e: Symex): Expr[AstType] = Expr.Atom(this)
  val shht = new Builtin("shht"):
    def expand(e: Symex): Expr[AstType] = Expr.Atom(this)
  language("langHello", hello, shht)

def demo() =
  println("Macro compiler")
  run(new UntypedProgram("foo", langHello, List(Expr.Atom("hello"), Expr.Atom("hello"))))
