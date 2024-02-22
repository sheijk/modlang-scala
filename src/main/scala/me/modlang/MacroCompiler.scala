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
    case Leaf(id) => s"S($id)"
    case Node(args) => args.map(_.toString()).mkString("L(", " ", ")")
type SymEx = Tree[String]

object SymEx:
  def sym(x: String|SymEx): SymEx = x match { case s: String => Tree.Leaf(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = Tree.Node(args.map(sym).toList)
  def seq(args: (String|SymEx)*) = Tree.Node(Tree.Leaf("seq") :: args.map(sym).toList)
  def hello = sym("hello")
  def helloJan = sym("helloJan")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")

trait BuiltinBase[OutEx]:
  type Context
  type InEx
  val name: String
  def create(ctx: Context, expr: InEx): Option[OutEx]

abstract class Builtin[Context_, InEx_, OutEx] extends BuiltinBase[OutEx]:
  type Context = Context_
  type InEx = InEx_

abstract class Macro:
  val name: String
  def expand(ex: SymEx): SymEx

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
  type MacroBuiltin = Builtin[Context, SymEx, OutEx]
  def initBuiltins(): List[MacroBuiltin]

  def initMacros(): List[Macro]
  val globals: Scope[Symbol] =
    val table = SimpleScope[Symbol](None)
    initBuiltins().foreach(b => table.register(b.name, b))
    initMacros().foreach(m => table.register(m.name, m))
    table

  case class NotFound()
  type Symbol = MacroBuiltin|Macro|NotFound

  def lookup(id: String): Symbol =
    globals.lookup(id).getOrElse(NotFound())

  def compilerError(msg: String): OutEx
  def translateList(ctx: Context, exs: List[I]): OutEx
  def translate(ctx: Context, ex: I): O =
    def translate1(id: String): O =
      lookup(id) match
      case m: Macro => translate(ctx, m.expand(ex))
      case b: MacroBuiltin => b.create(ctx, ex).getOrElse(compilerError(s"Builtin expression is invalid $id"))
      case _: NotFound => compilerError(s"Unknown id $id")
    ex match
    case Tree.Node(Tree.Leaf("seq") :: args) => translateList(ctx, args)
    case Tree.Node(Tree.Leaf(id) :: args) => translate1(id)
    case Tree.Leaf(id) => translate1(id)
    case Tree.Node(exs) => translateList(ctx, exs)

object MacroLanguage:
  val helloJan = new Macro:
    val name: String = "helloJan"
    def expand(ex: SymEx): SymEx = SymEx.l(SymEx.l("name", "Jan"), "hello")

case class HelloLanguage() extends MacroLanguage[Program]:
  def initMacros(): List[Macro] = List(MacroLanguage.helloJan)
  def initBuiltins() = List(helloB, shhhtB, nameB)

  case class Context(scope: SimpleScope[Symbol]) extends ContextI:
    def symbols() = scope

    var silent = false
    var name: Option[String] = None
    def hello(): Value =
      (silent, name) match
        case (true, _) => List()
        case (false, None) => "hello!"
        case (false, Some(name)) => s"hello $name!"
  def initialContext() = Context(SimpleScope[Symbol](None))

  def compilerError(msg: String): Program =
    () => s"error: $msg"

  def translateList(ctx: Context, exs: List[I]) =
    val nestedCtx = ctx.copy(scope = SimpleScope[Symbol](Some(ctx.scope)))
    val outs: List[Value] = exs.map(translate(nestedCtx, _)())
    def toValues(v: Value): List[SingleValue] =
      v match
      case l: List[SingleValue] => l
      case s: SingleValue => List(s)
    val flat: List[SingleValue] = outs.flatMap(toValues)
    () => flat

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
  val helloL = HelloLanguage()
  import SymEx.*
  helloL.runAndPrint(sym("hello"))
  helloL.runAndPrint(seq("hello", "hello", "hello"))
  helloL.runAndPrint(seq("hello", "shhht", "hello"))
  helloL.runAndPrint(seq(name("foo"), hello, hello, name("bar"), hello, shhht, hello, hello))
  helloL.runAndPrint(seq(l(helloJan), hello))
