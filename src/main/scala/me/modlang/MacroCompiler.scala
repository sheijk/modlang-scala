package me
package modlang
package macro_compiler

trait Language[InEx, OutEx]:
  type I = InEx
  type O = OutEx

  type Context
  def initialContext(): Context

  def translate(ctx: Context, ex: InEx): OutEx
  def translateAll(ctx: Context, exs: List[InEx]): List[OutEx] =
    val ctx = initialContext()
    exs.map(translate(ctx, _))

extension (lang: Language[SymEx, Program])
  def compile(ast: lang.I): lang.O =
    lang.translate(lang.initialContext(), ast)

  def runAndPrint(ast: lang.I) =
    val program = lang.compile(ast)
    val result = program()
    println(s"eval($ast) = $result")

type SingleValue = Int | Boolean | String
type Value = SingleValue | List[SingleValue]
type Program = () => Value

enum Tree[T]:
  case Leaf(v: T)
  case Node(l: List[Tree[T]])
  def id() = this match { case Leaf(name) => Some(name) case Node(Leaf(name) :: _) => Some(name) case _ => None }
  override def toString() = this match
    case Leaf(id) => s"S($id)"
    case Node(args) => args.map(_.toString()).mkString("L(", " ", ")")
type SymEx = Tree[String]

object SymEx:
  def sym(x: String|SymEx): SymEx = x match { case s: String => Tree.Leaf(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = Tree.Node(args.map(sym).toList)
  def hello = sym("hello")
  def helloJan = sym("helloJan")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")

trait Builtin[Context, InEx, OutEx]:
  val name: String
  def create(ctx: Context, expr: InEx): Option[OutEx]

trait Macro:
  val name: String
  def expand(ex: SymEx): SymEx

trait SymbolTable[Symbol]:
  trait Scope:
    def lookup(id: String): Symbol
    def add(id: String, sym: Symbol): Unit
    def subScope(): Scope

  def initial(): Scope

trait MacroLanguage[Context, OutEx](builtins: List[Builtin[Context, SymEx, OutEx]]) extends Language[SymEx, OutEx]:
  val builtinMap: Map[String, Builtin[Context, SymEx, OutEx]] = builtins.map(b => (b.name, b)).toMap

  def initMacros(): List[Macro]
  val macroMap = scala.collection.mutable.Map[String, Macro](initMacros().map(m => (m.name, m))*)

  def translateBuiltin(ctx: Context, ast: SymEx): Option[OutEx] =
    ast.id().flatMap(builtinMap get _).flatMap(_.create(ctx, ast))

  def compilerError(msg: String): OutEx
  def translateList(ctx: Context, exs: List[SymEx]): OutEx
  def translate(ctx: Context, ex: SymEx): O =
    translateBuiltin(ctx, ex).getOrElse(
      ex.id().flatMap(macroMap get _).map(m => translate(ctx, m.expand(ex))).getOrElse(
      ex match
        case Tree.Node(exs) => translateList(ctx, exs)
        case Tree.Leaf(id) => compilerError(s"Unknown ast id $id")))

object MacroLanguage:
  val helloJan = new Macro:
    val name: String = "helloJan"
    def expand(ex: SymEx): SymEx = SymEx.l(SymEx.l("name", "Jan"), "hello")

case class HelloLanguage() extends MacroLanguage[HelloLanguage.Context, Program](HelloLanguage.builtins):
  def initMacros(): List[Macro] = List(MacroLanguage.helloJan)

  type Context = HelloLanguage.Context
  def initialContext() = HelloLanguage.Context()

  def compilerError(msg: String): Program =
    () => s"error: $msg"

  def translateList(ctx: Context, exs: List[SymEx]) =
    val nestedCtx = ctx.copy()
    val outs: List[Value] = exs.map(translate(nestedCtx, _)())
    def toValues(v: Value): List[SingleValue] =
      v match
      case l: List[SingleValue] => l
      case s: SingleValue => List(s)
    val flat: List[SingleValue] = outs.flatMap(toValues)
    () => flat

object HelloLanguage:
  case class Context():
    var silent = false
    var name: Option[String] = None
    def hello(): Value =
      (silent, name) match
        case (true, _) => List()
        case (false, None) => "hello!"
        case (false, Some(name)) => s"hello $name!"
  type HelloBuiltin = Builtin[Context, SymEx, Program]
  val helloB = new HelloBuiltin:
    val name: String = "hello"
    def create(ctx: Context, expr: SymEx): Option[Program] =
      expr match
        case Tree.Leaf(_) =>
          val msg = ctx.hello()
          Some(() => msg)
        case _ => None
  val shhhtB = new HelloBuiltin:
    val name: String = "shhht"
    def create(ctx: Context, expr: SymEx): Option[Program] =
      ctx.silent = true
      Some(() => List())
  val nameB = new HelloBuiltin:
    val name: String = "name"
    def create(ctx: Context, expr: SymEx): Option[Program] =
      expr match
        case Tree.Node(List(Tree.Leaf(_), Tree.Leaf(name))) =>
          ctx.name = Some(name)
          Some(() => List())
        case _ => None
  val builtins: List[Builtin[Context, SymEx, Program]] = List(helloB, shhhtB, nameB)

def demo() =
  println("Macro compiler")
  val helloL = HelloLanguage()
  import SymEx.*
  helloL.runAndPrint(sym("hello"))
  helloL.runAndPrint(l("hello", "hello", "hello"))
  helloL.runAndPrint(l("hello", "shhht", "hello"))
  helloL.runAndPrint(l(name("foo"), hello, hello, name("bar"), hello, shhht, hello, hello))
  helloL.runAndPrint(l(l(helloJan), hello))
