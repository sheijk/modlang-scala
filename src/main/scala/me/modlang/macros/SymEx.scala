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

  def bindIdsInPattern(pattern: SymEx): Either[List[String], List[(String, SymEx)]] =
    val errors = scala.collection.mutable.ListBuffer[String]()
    def f(ex: SymEx, pattern: SymEx): List[(String, SymEx)] =
      (ex, pattern) match
      case (Tree.Node(childs), Tree.Node(subPatterns)) =>
        if childs.length == subPatterns.length then
          childs.zip(subPatterns).flatMap(f)
        else
          errors.addOne(s"length mismatch in $childs")
          List()
      case (_, Tree.Leaf(id @ s"$$$_")) =>
        List((id, ex))
      case (Tree.Leaf(exName), Tree.Leaf(patternName)) if exName == patternName =>
        List()
      case _ =>
        errors.addOne(s"failed in $ex")
        List()
    val r = f(ex, pattern)
    if errors.length == 0 then
      Right(r)
    else
      Left(errors.toList)


object SymEx:
  def sym(x: String|SymEx): SymEx = x match { case s: String => Tree.Leaf(s) case x: SymEx => x}
  def l(args: (String|SymEx)*) = Tree.Node(args.map(sym).toList)
  def seq(args: (String|SymEx)*) = Tree.Node(Tree.Leaf("seq") :: args.map(sym).toList)
  def fromList(lst: List[SymEx]) =
    lst match
    case List(one) => one
    case _ => l(lst*)
  def hello = sym("hello")
  def name(n: String) = l("name", n)
  def shhht = sym("shhht")
  def helloJan = sym("helloJan")
  def janMode = sym("janMode")
  def nothing = l()
  def error(msg: String, ex: SymEx): SymEx = SymEx.l("error", msg, ex)

  object ParseRules:
    import ParseUtils.*
    def atom: Prop[SymEx] = (id.map(SymEx.sym)) <* wsMaybe
    def unparenthized: Prop[SymEx] = (atom | parenthized).repeat.map(SymEx.fromList)
    // infinite loop here if we keep this in point free style
    def parenthized(p: ParseState): Parsed[SymEx] =
      (chr('(') *> (atom | parenthized).repeat.map(SymEx.l) <* chr(')') <* wsMaybe)(p)
    def symex: Prop[SymEx] = parenthized | unparenthized

  def parse(str: String): Either[ParseUtils.Error, SymEx] =
    ParseRules.symex(ParseUtils.ParseState(str))

object ParseUtils:
  case class Error(msg: String, index: Int)

  case class ParseState(val str: String, var idx: Int = 0)
  type Parsed[T] = Either[Error, T]
  type Prop[T] = ParseState => Parsed[T]

  def ok[T](x: T): Prop[T] = _ => Right(x)
  def err(msg: String) = (p: ParseState) => Left(Error(msg, p.idx))
  def inBounds(p: ParseState) = p.idx < p.str.length
  def any: Prop[Char] = p =>
    if inBounds(p) then
      val chr = p.str(p.idx)
      p.idx = p.idx + 1
      ok(chr)(p)
    else
      err("end of input")(p)
  def chr(expect: Char): Prop[Unit] = p =>
    any(p).flatMap(x =>
    if x == expect
      then ok(())(p)
      else
        val found = if inBounds(p) then x else "end of input"
        err(s"Expected '$expect' but found '$found'")(p))
  def endOfInput: Prop[Unit] = p =>
    if inBounds(p)
    then err("not and end of input")(p)
    else ok(())(p)
  def id: Prop[String] = substringUntil(ws | endOfInput | (chr('(') | chr(')')).ignore).notEmpty
  def wsChar: Prop[Unit] = chr(' ') | chr('\n') | chr('\t')
  def ws: Prop[Unit] = wsChar.oneOrMore.ignore
  def wsMaybe: Prop[Unit] = wsChar.repeat.ignore
  def substringUntil[T](f: Prop[T]): Prop[String] = p =>
    val start = p.idx
    def step(): Parsed[String] =
      val prevIdx = p.idx
      val r = f(p)
      r match
        case Left(e) =>
          if prevIdx < p.str.length then
            if prevIdx == p.idx then
              p.idx = p.idx + 1
            step()
          else
            err("Unexpected end of input")(p)
        case Right(_) =>
          val s = p.str.substring(start, prevIdx)
          p.idx = prevIdx
          if s == null
          then Right("")
          else Right(s.asInstanceOf[String])
    step()
  extension (lhs: Prop[String]) def notEmpty: Prop[String] = p =>
    lhs(p).flatMap(s => if s.length == 0 then err("Expected non empty string")(p) else Right(s))
  extension[T] (lhs: Prop[T])
    def ignore: Prop[Unit] = p => lhs(p).map(_ => ())
    def |>[U](rhs: Prop[U]): Prop[(T, U)] = p =>
      lhs(p).flatMap((lval: T) => rhs(p).map((rval: U) => (lval, rval)))
    def *>[U](rhs: Prop[U]): Prop[U] = p =>
      lhs(p).flatMap((_: T) => rhs(p).map((rval: U) => rval))
    def <*[U](rhs: Prop[U]): Prop[T] = p =>
      lhs(p).flatMap((lval: T) => rhs(p).map((_: U) => lval))
    def |(rhs: Prop[T]): Prop[T] = p =>
      val start = p.idx
      def retry() =
        p.idx = start
        rhs(p)
      lhs(p).orElse(retry()).orElse(err("no alternative matched")(p))
    def repeat: Prop[List[T]] = p =>
      def step(): List[T] =
        val start = p.idx
        lhs(p) match
        case Left(_) =>
          p.idx = start
          List()
        case Right(expr) =>
          if p.idx == start then
            List(expr)
          else
            expr :: step()
      Right(step())
    def oneOrMore: Prop[List[T]] = (lhs |> lhs.repeat).map(headTail => headTail._1 :: headTail._2)
    def map[U](f: T => U): Prop[U] = p => lhs(p).map(f)
