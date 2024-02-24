package me
package modlang
package macro_compiler

trait MacroLanguage[OutEx] extends Language[SymEx, OutEx]:
  type Context <: ContextI
  trait ContextI:
    def symbols(): Scope[Symbol]
    def subContext(): Context
  type MacroContext = Context

  type MacroBuiltin = Builtin[Context, SymEx, OutEx]
  def initBuiltins(): List[MacroBuiltin]
  def initMacros(): List[Macro]

  val globals: Scope[Symbol] =
    val table = SimpleScope[Symbol](None)
    initBuiltins().foreach(b => table.register(b.name, b))
    initMacros().foreach(m => table.register(m.name, m))
    table

  type Symbol = MacroBuiltin|Macro|NotFound
  // not a trait because this is used in a union type for run-time type-checking
  abstract class Macro:
    val name: String
    def expand(ctx: Context, ex: SymEx): SymEx
  case class NotFound()

  def compilerError(msg: String, ex: SymEx): OutEx
  def translateList(ctx: Context, exs: List[I]): OutEx
  def translate(ctx: Context, ex: I): O =
    def translate1(id: String): O =
      ctx.symbols().lookup(id) match
      case Some(m: Macro) =>
        translate(ctx, m.expand(ctx, ex))
      case Some(b: MacroBuiltin) =>
        b.create(ctx, ex).getOrElse(
          compilerError(s"Builtin expression $id is invalid", ex))
      case None | Some(_: NotFound) =>
        compilerError(s"Unknown id $id", ex)
    ex match
    case Tree.Node(Tree.Leaf("error") :: Tree.Leaf(msg) :: args ) => compilerError(msg, Tree.Node(args))
    case Tree.Node(Tree.Leaf("seq") :: args) => translateList(ctx, args)
    case Tree.Node(Tree.Leaf(id) :: _) => translate1(id)
    case Tree.Leaf(id) => translate1(id)
    case Tree.Node(exs) => translateList(ctx, exs)

  def symbolMacro(mname: String, repl: List[SymEx]) = new Macro:
    val name = mname
    def expand(ctx: Context, ex: SymEx): SymEx =
      ex match
      case Tree.Node(List(_)) | Tree.Leaf(_) =>
        repl match
        case List(single) => single
        case _ => Tree.Node(repl)
      case _ =>
        SymEx.error("Expected 0 arguments", ex)

  def replacementMacro(mname: String, pattern: SymEx, repl: List[SymEx]) = new Macro:
    val name = mname
    def expand(ctx: Context, ex: SymEx): SymEx =
      ex.bindIdsInPattern(pattern) match
      case Right(replacements) =>
        val newExs = repl.map(_.replace(replacements))
        val r = Tree.Node(newExs)
        println(s"Macro $pattern\n  in $ex\n  matches $replacements\n  to $repl\n  result $r")
        r
      case _ => ???

  def defmacro = new Macro:
    type Context = MacroContext
    val name: String = "defmacro"
    def expand(ctx: Context, ex: SymEx): SymEx =
      ex match
      case Tree.Node(Tree.Leaf(_) :: Tree.Node(List(Tree.Leaf(mname))) :: repl) =>
        ctx.symbols().register(mname, symbolMacro(mname, repl))
        SymEx.nothing
      case Tree.Node(Tree.Leaf(_) :: (pattern @ Tree.Node(Tree.Leaf(mname) :: _)) :: repl) =>
        ctx.symbols().register(mname, replacementMacro(mname, pattern, repl))
        SymEx.nothing
      case _ =>
        SymEx.error("Expected (defmacro (name [args:id ...]))", ex)

