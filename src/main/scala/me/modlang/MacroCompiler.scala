package me
package modlang
package macro_compiler

type Value = Int | Boolean

opaque type BaseExpr = String

sealed trait AstType[Output]
trait Builtin[Output](val name: String) extends AstType[Output]:
  def expand(e: Symex): Output
  override def toString() = s"Builtin $name"
trait Macro[Output](val name: String) extends AstType[Output]

enum Expr[Head]:
  case Atom(hd: Head)
  case List(childs: List[Expr[Head]])
type Symex = Expr[String]

case class Language[Output](name: String, builtins: Map[String, Builtin[Output]]):
  def lookup(id: String): Option[Builtin[Output]] =
    builtins get id
  def expand(e: Symex): Output =
    e match
      case Expr.Atom(hd) =>
        lookup(hd) match
          case Some(builtin : Builtin[Output]) => builtin.expand(e)

  def compile(p: List[Symex]): List[Output] =
    p.map(expand(_))

def language[Output](name: String, builtins: Builtin[Output]*): Language[Output] =
  Language(name, builtins.map((b: Builtin[Output]) => (b.name, b)).toMap)

case class Program[Ex](
  name: String,
  language: Language[Ex],
  exprs: List[Ex],
):
  override def toString(): String =
    val body = exprs.map(_.toString).mkString("\n  ")
    s"Program $name :language ${language.name} {\n  $body\n}"

type OutputLang = () => Unit
type UntypedProgram = List[Symex]
type TypedProgram = List[OutputLang]

def run(l: Language[OutputLang], ast: List[Symex]) =
  val out: List[OutputLang] = l.compile(ast)
  out.foreach((program: OutputLang) =>
    println(s"Running $ast")
    program())

val langHello =
  val hello = new Builtin[OutputLang]("hello"):
    def expand(e: Symex): OutputLang = () => println("Hello!")
  val shht = new Builtin[OutputLang]("shht"):
    def expand(e: Symex): OutputLang = () => println("shht!")
  language("langHello", hello, shht)

def demo() =
  println("Macro compiler")
  run(langHello, List(Expr.Atom("hello"), Expr.Atom("hello")))
