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

trait BuiltinBase[OutEx]:
  type Context
  type InEx
  val name: String
  def create(ctx: Context, expr: InEx): Option[OutEx]

// not a trait because this is used in a union type for run-time type-checking
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

