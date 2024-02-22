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

type SingleValue = Int | Boolean | String
type Value = SingleValue | List[SingleValue]
type Program = () => Value

enum SymEx:
  case S(v: String)
  case Ls(l: List[SymEx])
  def id() = this match { case S(name) => Some(name) case _ => None }

def compile(lang: Language[SymEx, Program], ast: lang.I): lang.O =
  lang.translate(lang.initialContext(), ast)

def runAndPrint(lang: Language[SymEx, Program], ast: lang.I) =
  val program = compile(lang, ast)
  val result = program()
  println(s"eval($ast) = $result")

trait Builtin[Context, InEx, OutEx]:
  val name: String
  def create(ctx: Context, expr: InEx): Option[OutEx]

trait MacroLanguage[Context, OutEx](builtins: List[Builtin[Context, SymEx, OutEx]]) extends Language[SymEx, OutEx]:
  val builtinMap: Map[String, Builtin[Context, SymEx, OutEx]] = builtins.map(b => (b.name, b)).toMap

  def translateBuiltin(ctx: Context, ast: SymEx): Option[OutEx] =
    ast.id().flatMap(builtinMap get _).flatMap(_.create(ctx, ast))

  def compilerError(msg: String): OutEx
  def translate(ctx: Context, ex: SymEx): O =
    translateBuiltin(ctx, ex).getOrElse(compilerError(s"Unknown ast id ${ex.id().getOrElse("*List*")}"))

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
    def create(ctx: Context, expr: SymEx): Option[Program] = ???
  val shhhtB = new HelloBuiltin:
    val name: String = "shhht"
    def create(ctx: Context, expr: SymEx): Option[Program] = ???
  val nameB = new HelloBuiltin:
    val name: String = "name"
    def create(ctx: Context, expr: SymEx): Option[Program] = ???
  val builtins: List[Builtin[Context, SymEx, Program]] = List(helloB, shhhtB, nameB)

case class HelloLanguage() extends MacroLanguage[HelloLanguage.Context, Program](HelloLanguage.builtins):
  val nothing: Value = List()
  type Context = HelloLanguage.Context
  def initialContext() = HelloLanguage.Context()

  def compilerError(msg: String): Program = () => s"error: $msg"
  override def translate(ctx: Context, ex: I): Program =
    ex match
      case SymEx.S("hello") =>
        val msg = ctx.hello()
        () => msg
      case SymEx.S("shhht") =>
        ctx.silent = true
        () => nothing
      case SymEx.Ls(List(SymEx.S("name"), SymEx.S(name))) =>
        ctx.name = Some(name)
        () => nothing
      case SymEx.Ls(exs) =>
        val nestedCtx = ctx.copy()
        val outs: List[Value] = exs.map(translate(nestedCtx, _)())
        def toValues(v: Value): List[SingleValue] =
          v match
          case l: List[SingleValue] => l
          case s: SingleValue => List(s)
        val flat: List[SingleValue] = outs.flatMap(toValues)
        () => flat
      case _ =>
        super.translate(ctx, ex)

def demo() =
  println("Macro compiler")
  val helloL = HelloLanguage()
  def sym(x: String|SymEx): SymEx = x match { case s: String => SymEx.S(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = SymEx.Ls(args.map(sym).toList)
  def hello = sym("hello")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")
  runAndPrint(helloL, sym("hello"))
  runAndPrint(helloL, l("hello", "hello", "hello"))
  runAndPrint(helloL, l("hello", "shhht", "hello"))
  runAndPrint(helloL, l(name("foo"), hello, hello, name("bar"), hello, shhht, hello, hello))
