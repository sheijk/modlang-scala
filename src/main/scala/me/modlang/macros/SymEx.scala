package me
package modlang
package macro_compiler

enum Tree[+T]:
  case Leaf(v: T)
  case Node(l: List[Tree[T]])
  def id() = this match { case Leaf(name) => Some(name) case Node(Leaf(name) :: _) => Some(name) case _ => None }
  override def toString() = this match
    case Leaf(id) => s"$id"
    case Node(args) => args.map(_.toString()).mkString("(", " ", ")")

case class MatchError(msg: String, ex: SymEx, pattern: SymEx)

type SymEx = Tree[String]
extension (ex: SymEx)
  def replace(replacements: List[(String, SymEx)]): SymEx =
    ex match
    case Tree.Leaf(name) =>
      replacements.filter(name == _._1) match
      case List((_, replacement)) => replacement
      case _ => ex
    case Tree.Node(childs) =>
      Tree.Node(childs.map(_.replace(replacements)))
  def bindIdsInPattern(pattern: SymEx): List[(String, SymEx)] =
    (ex, pattern) match
    case (Tree.Node(childs), Tree.Node(subPatterns)) =>
      if childs.length == subPatterns.length then
        childs.zip(subPatterns).flatMap((c, p) => c.bindIdsInPattern(p))
      else
        List()
    case (_, Tree.Leaf(id @ s"$$$_")) =>
      List((id, ex))
    case _ =>
      List()

object SymEx:
  def sym(x: String|SymEx): SymEx = x match { case s: String => Tree.Leaf(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = Tree.Node(args.map(sym).toList)
  def seq(args: (String|SymEx)*) = Tree.Node(Tree.Leaf("seq") :: args.map(sym).toList)
  def hello = sym("hello")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")
  def helloJan = sym("helloJan")
  def janMode = sym("janMode")
  def nothing = l()
  def error(msg: String, ex: SymEx): SymEx = SymEx.l("error", msg, ex)

