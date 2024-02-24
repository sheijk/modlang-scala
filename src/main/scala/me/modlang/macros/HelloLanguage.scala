package me
package modlang
package macro_compiler

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

  def compilerError(msg: String, ex: SymEx): Program =
    () => s"error: $msg in $ex"

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

