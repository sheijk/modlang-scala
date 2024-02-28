package me
package modlang
package macro_compiler

trait MacroLanguage[OutEx] extends Language[SymEx, OutEx]:
  type Context <: ContextI
  trait ContextI:
    def symbols(): Scope[Symbol]
    def subContext(): Context
  type MacroContext = Context

  def initBuiltins(): List[Builtin]
  def initMacros(): List[Macro]

  val globals: Scope[Symbol] =
    val table = SimpleScope[Symbol](None)
    initBuiltins().foreach(b => table.register(b.name, Symbol.B(b)))
    initMacros().foreach(m => table.register(m.name, Symbol.M(m)))
    table

  trait Builtin:
    val name: String
    def create(ctx: Context, expr: MacroEx): Option[OutEx]

  trait Macro:
    val name: String
    def expand(ctx: Context, ex: MacroEx): MacroEx

  enum Symbol:
    case B(b: Builtin)
    case M(m: Macro)
    case NotFound

  type MacroEx = Tree[String|Symbol]
  extension (sym: String|Symbol) def toStrSym =
    sym match
    case str: String => str
    case Symbol.M(m) => m.name
    case Symbol.B(b) => b.name
    case Symbol.NotFound => "not found"

  def toSymEx(ex: MacroEx): SymEx =
    ex match
    case Tree.Leaf(id) => Tree.Leaf(id.toStrSym)
    case Tree.Node(childs) => Tree.Node(childs.map(toSymEx(_)))

  def lookupSymbol(ctx: Context, sym: String|Symbol): Symbol =
    sym match
    case id: String =>
      ctx.symbols().lookup(id).getOrElse(Symbol.NotFound)
    case sym: Symbol =>
      sym

  def compilerError(msg: String, ex: MacroEx): OutEx
  def translateList(ctx: Context, exs: List[MacroEx]): OutEx
  def translate(ctx: Context, ex: SymEx): O = translateI(ctx, ex)
  def translateI(ctx: Context, ex: MacroEx): O =
    def translate1(id: String|Symbol): O =
      lookupSymbol(ctx, id) match
      case Symbol.M(m) =>
        translateI(ctx, m.expand(ctx, ex))
      case Symbol.B(b) =>
        b.create(ctx, ex).getOrElse(
          compilerError(s"Builtin expression $id is invalid", ex))
      case Symbol.NotFound =>
        compilerError(s"Unknown id $id", ex)
    ex match
    case Tree.Node(Tree.Leaf("error") :: Tree.Leaf(msg: String) :: args ) => compilerError(msg, Tree.Node(args))
    case Tree.Node(Tree.Leaf("seq") :: args) => translateList(ctx, args)
    case Tree.Node(Tree.Leaf(id) :: _) => translate1(id)
    case Tree.Leaf(id) => translate1(id)
    case Tree.Node(exs) => translateList(ctx, exs)

  def symbolMacro(mname: String, repl: List[MacroEx]) = new Macro:
    val name = mname
    def expand(ctx: Context, ex: MacroEx): MacroEx =
      ex match
      case Tree.Node(List(_)) | Tree.Leaf(_) =>
        repl match
        case List(single) => single
        case _ => Tree.Node(repl)
      case _ =>
        SymEx.error("Expected 0 arguments", toSymEx(ex))

  def replacementMacro(mname: String, pattern: MacroEx, repl: List[MacroEx]) = new Macro:
    val name = mname
    def expand(ctx: Context, ex: MacroEx): MacroEx =
      ex.bindIdsInPattern(toSymEx(pattern)) match
      case Right(replacements) =>
        val newExs = repl.map(_.replace(replacements))
        val r = Tree.Node(newExs)
        println(s"Macro $pattern\n  in $ex\n  matches $replacements\n  to $repl\n  result $r")
        r
      case Left(_) =>
        SymEx.error(s"Expected $pattern", toSymEx(ex))

  def defmacro = new Macro:
    val name: String = "defmacro"
    def expand(ctx: Context, ex: MacroEx): SymEx =
      ex match
      case Tree.Node(Tree.Leaf(_) :: Tree.Node(List(Tree.Leaf(mname: String))) :: repl) =>
        ctx.symbols().register(mname, Symbol.M(symbolMacro(mname, repl)))
        SymEx.nothing
      case Tree.Node(Tree.Leaf(_) :: (pattern @ Tree.Node(Tree.Leaf(mname: String) :: _)) :: repl) =>
        ctx.symbols().register(mname, Symbol.M(replacementMacro(mname, pattern, repl)))
        SymEx.nothing
      case _ =>
        SymEx.error("Expected (defmacro (name [args:id ...]))", toSymEx(ex))

