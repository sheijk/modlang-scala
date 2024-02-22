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

enum SymEx:
  case S(v: String)
  case L(l: List[SymEx])
  def id() = this match { case S(name) => Some(name) case L(S(name) :: _) => Some(name) case _ => None }
  override def toString() = this match
    case S(id) => s"S($id)"
    case L(args) => args.map(_.toString()).mkString("L(", " ", ")")

object SymEx:
  def sym(x: String|SymEx): SymEx = x match { case s: String => SymEx.S(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = SymEx.L(args.map(sym).toList)
  def hello = sym("hello")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")

trait Builtin[Context, InEx, OutEx]:
  val name: String
  def create(ctx: Context, expr: InEx): Option[OutEx]

trait MacroLanguage[Context, OutEx](builtins: List[Builtin[Context, SymEx, OutEx]]) extends Language[SymEx, OutEx]:
  val builtinMap: Map[String, Builtin[Context, SymEx, OutEx]] = builtins.map(b => (b.name, b)).toMap

  def translateBuiltin(ctx: Context, ast: SymEx): Option[OutEx] =
    ast.id().flatMap(builtinMap get _).flatMap(_.create(ctx, ast))

  def compilerError(msg: String): OutEx
  def translateList(ctx: Context, exs: List[SymEx]): OutEx
  def translate(ctx: Context, ex: SymEx): O =
    translateBuiltin(ctx, ex).getOrElse(
      ex match
        case SymEx.L(exs) => translateList(ctx, exs)
        case SymEx.S(id) => compilerError(s"Unknown ast id $id"))

case class HelloLanguage() extends MacroLanguage[HelloLanguage.Context, Program](HelloLanguage.builtins):
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
        case SymEx.S(_) =>
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
        case SymEx.L(List(SymEx.S(_), SymEx.S(name))) =>
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
