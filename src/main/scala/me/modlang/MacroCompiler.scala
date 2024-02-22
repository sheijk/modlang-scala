package me
package modlang
package macro_compiler

import scala.Conversion

trait Language[InEx, OutEx]:
  type I = InEx
  type O = OutEx

  type Context
  def initialContext(): Context

  def translate(ctx: Context, ex: InEx): OutEx
  def translateAll(ctx: Context, exs: List[InEx]): List[OutEx] =
    val ctx = initialContext()
    exs.map(translate(ctx, _))

extension (lang: Language[?, Program])
  def compile(ast: lang.I): lang.O =
    lang.translate(lang.initialContext(), ast)

  def runAndPrint(ast: lang.I) =
    val program = lang.compile(ast)
    val result = program()
    println(s"eval($ast) = $result")

type SingleValue = Int | Boolean | String
type Value = SingleValue | List[SingleValue]
type Program = () => Value

enum Tree[+T]:
  case Leaf(v: T)
  case Node(l: List[Tree[T]])
  def id() = this match { case Leaf(name) => Some(name) case Node(Leaf(name) :: _) => Some(name) case _ => None }
  override def toString() = this match
    case Leaf(id) => s"$id"
    case Node(args) => args.map(_.toString()).mkString("(", " ", ")")
type SymEx = Tree[String]

object SymEx:
  def sym(x: String|SymEx): SymEx = x match { case s: String => Tree.Leaf(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = Tree.Node(args.map(sym).toList)
  def seq(args: (String|SymEx)*) = Tree.Node(Tree.Leaf("seq") :: args.map(sym).toList)
  def hello = sym("hello")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")
  def helloJan = sym("helloJan")
  def janMode = sym("janMode")

trait BuiltinBase[OutEx]:
  type Context
  type InEx
  val name: String
  def create(ctx: Context, expr: InEx): Option[OutEx]

abstract class Builtin[Context_, InEx_, OutEx] extends BuiltinBase[OutEx]:
  type Context = Context_
  type InEx = InEx_

trait Scope[Symbol]:
  def lookup(id: String): Option[Symbol]
  def register(id: String, sym: Symbol): Unit
  def subScope(): Scope[Symbol]

class SimpleScope[Symbol](parent: Option[Scope[Symbol]]) extends Scope[Symbol]:
  var table = Map[String, Symbol]()
  def lookup(id: String): Option[Symbol] =
    table get id match
      case Some(x) => Some(x)
      case None => parent.flatMap(_.lookup(id))
  def register(id: String, sym: Symbol): Unit =
    table = table + (id -> sym)
  def subScope(): Scope[Symbol] =
    SimpleScope[Symbol](Some(this))

enum MacroEx[OutEx]:
  case S(s: String)
  case B(b: BuiltinBase[OutEx])

given symexToMacro[T] : Conversion[SymEx, Tree[MacroEx[T]]] with
  def apply(x: SymEx): Tree[MacroEx[T]] =
  x match
    case Tree.Leaf(str) => Tree.Leaf(MacroEx.S(str))
    case Tree.Node(exs) => Tree.Node(exs.map(symexToMacro(_)))

trait MacroLanguage[OutEx] extends Language[SymEx, OutEx]:
  type Context <: ContextI
  trait ContextI:
    def symbols(): Scope[Symbol]
    def subContext(): Context
  type MacroBuiltin = Builtin[Context, SymEx, OutEx]
  def initBuiltins(): List[MacroBuiltin]

  abstract class Macro:
    val name: String
    def expand(ctx: Context, ex: SymEx): SymEx

  def initMacros(): List[Macro]
  val globals: Scope[Symbol] =
    val table = SimpleScope[Symbol](None)
    initBuiltins().foreach(b => table.register(b.name, b))
    initMacros().foreach(m => table.register(m.name, m))
    table

  case class NotFound()
  type Symbol = MacroBuiltin|Macro|NotFound

  def compilerError(msg: String): OutEx
  def translateList(ctx: Context, exs: List[I]): OutEx
  def translate(ctx: Context, ex: I): O =
    def translate1(id: String): O =
      ctx.symbols().lookup(id) match
      case Some(m: Macro) => translate(ctx, m.expand(ctx, ex))
      case Some(b: MacroBuiltin) => b.create(ctx, ex).getOrElse(compilerError(s"Builtin expression is invalid $id"))
      case None | Some(_: NotFound) => compilerError(s"Unknown id $id in $ex")
    ex match
    case Tree.Node(Tree.Leaf("seq") :: args) => translateList(ctx, args)
    case Tree.Node(Tree.Leaf(id) :: _) => translate1(id)
    case Tree.Leaf(id) => translate1(id)
    case Tree.Node(exs) => translateList(ctx, exs)

  type MacroContext = Context

  def helloJan = new Macro:
    type Context = MacroContext
    val name: String = "helloJan"
    def expand(ctx: Context, ex: SymEx): SymEx = SymEx.l(SymEx.l("name", "Jan"), "hello")

  def janMode = new Macro:
    type Context = MacroContext
    val name: String = "janMode"
    def expand(ctx: Context, ex: SymEx): SymEx =
      ctx.symbols().register(helloJan.name, helloJan)
      SymEx.l()

  def symexError(msg: String, ex: SymEx): SymEx = SymEx.l("error", msg, ex)
  def defmacro = new Macro:
    type Context = MacroContext
    val name: String = "defmacro"
    def expand(ctx: Context, ex: SymEx): SymEx =
      ex match
      case Tree.Node(Tree.Leaf(_) :: Tree.Node(Tree.Leaf(mname) :: params) :: repl) =>
        def newMacro = new Macro:
          val name = mname
          def toIdOrError(ex: SymEx): String =
            ex match { case Tree.Leaf(n) => n case _ => ??? }
          val paramNames = params.map(toIdOrError)
          def expand(ctx: Context, ex: SymEx): SymEx =
            ex match
            case Tree.Node(List(_)) | Tree.Leaf(_) =>
              repl match
              case List(single) => single
              case _ => Tree.Node(repl)
            case Tree.Node(Tree.Leaf(_) :: args) =>
              if args.length != params.length then
                symexError(s"Expected ${params.length} arguments but found ${args.length}", ex)
              else
                def replace(ex: SymEx, replacements: List[(String, SymEx)]): SymEx =
                  ex match
                  case Tree.Leaf(name) =>
                    replacements.filter(name == "$" + _._1) match
                    case List((_, replacement)) => replacement
                    case _ => ex
                  case Tree.Node(childs) =>
                    Tree.Node(childs.map(replace(_, replacements)))
                val replacements = paramNames.zip(args)
                Tree.Node(repl.map(replace(_, replacements)))
            case Tree.Node(Tree.Node(_) :: _) | Tree.Node(List()) =>
              ???
        ctx.symbols().register(mname, newMacro)
        Tree.Node(List())
      case _ => ???

case class HelloLanguage() extends MacroLanguage[Program]:
  def initMacros(): List[Macro] = List(janMode, defmacro)
  def initBuiltins() = List(helloB, shhhtB, nameB)

  case class Context(scope: Scope[Symbol]) extends ContextI:
    def symbols() = scope
    def subContext(): Context =
      this.copy(scope = this.scope.subScope())

    var silent = false
    var name: Option[String] = None
    def hello(): Value =
      (silent, name) match
        case (true, _) => List()
        case (false, None) => "hello!"
        case (false, Some(name)) => s"hello $name!"
  def initialContext() = Context(globals)

  def compilerError(msg: String): Program =
    () => s"error: $msg"

  def translateList(ctx: Context, exs: List[I]) =
    val nestedCtx = ctx.subContext()
    val outs: List[Program] = exs.map(translate(nestedCtx, _))
    def toValues(v: Value): List[SingleValue] =
      v match
      case l: List[SingleValue] => l
      case s: SingleValue => List(s)
    () =>
      outs.flatMap(p => toValues(p()))

  type HelloBuiltin = MacroBuiltin
  def helloB = new HelloBuiltin:
    val name: String = "hello"
    def create(ctx: this.Context, expr: SymEx): Option[Program] =
      expr match
        case Tree.Leaf(_) =>
          val msg = ctx.hello()
          Some(() => msg)
        case _ => None
  def shhhtB = new HelloBuiltin:
    val name: String = "shhht"
    def create(ctx: this.Context, expr: SymEx): Option[Program] =
      ctx.silent = true
      Some(() => List())
  def nameB = new HelloBuiltin:
    val name: String = "name"
    def create(ctx: this.Context, expr: SymEx): Option[Program] =
      expr match
        case Tree.Node(List(Tree.Leaf(_), Tree.Leaf(name))) =>
          ctx.name = Some(name)
          Some(() => List())
        case _ => None

def demo() =
  println("Macro compiler")
  def helloL = HelloLanguage()
  import SymEx.*
  helloL.runAndPrint(sym("hello"))
  helloL.runAndPrint(seq("hello", "hello", "hello"))
  helloL.runAndPrint(seq("hello", "shhht", "hello"))
  helloL.runAndPrint(seq(name("foo"), hello, hello, name("bar"), hello, shhht, hello, hello))
  helloL.runAndPrint(seq(janMode, helloJan, hello))
  helloL.runAndPrint(seq(seq(janMode, helloJan), helloJan))

  helloL.runAndPrint(seq(
      l("defmacro", l("hifoo"), l(name("foo"), hello)),
      l("hifoo")))
  def greet(n: String) = l(name(n), hello)
  helloL.runAndPrint(seq(
      l("defmacro", l("swap", "left", "right"), "$right", "$left"),
      l("swap", greet("2nd"), greet("1st"))))
