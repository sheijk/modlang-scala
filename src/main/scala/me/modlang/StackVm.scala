package me
package modlang
package stackvm

import adt_interpreter.*

enum Token:
  case Number(value: Int)
  case Bool(value : Boolean)
  case Op(name : String)
  case Nop

def toToken(str : String) : Token =
  str.toIntOption match
    case Some(i) =>
      Token.Number(i)
    case None =>
      str match
      case "" => Token.Nop
      case "true" => Token.Bool(true)
      case "false" => Token.Bool(false)
      case _ => Token.Op(str)

def repl() =
  import scala.io.StdIn.readLine
  print("> ")
  val source = readLine()
  println()
  source match
    case "exit" | "quit" | "q" => ()
    case source => runSource(source)

def runSource(source : String) =
  val stack = evalSource(source)
  print("Got ")
  stack.foreach(x => print(s" $x"))
  println(s" by running $source")

def evalSource(source : String) =
  val stringTokens = source.split(" +") match
    case l: Array[String|Null] =>
      l.map(x => x match
        case x : String => x
        case _ => throw Error("null token"))
    case _ => throw Error("lexer error")
  evalTokens(stringTokens.map(toToken))

def evalTokens(tokens : Seq[Token]) : Seq[Value] =
  var stack : List[Value] = List()

  def takeTwo() : (Value, Value) =
    stack match
    case rhs :: lhs :: rem =>
      stack = rem
      (lhs, rhs)
    case _ =>
      throw Error("stack underflow" + stack)

  def applyStack(fn : (Expr, Expr) => Expr) =
    val (lhs, rhs) = takeTwo()
    stack = interprete(fn(Expr.Constant(lhs), Expr.Constant(rhs))) :: stack

  for (token <- tokens)
    token match
      case Token.Number(i) =>
        stack = Value.I(i) :: stack
      case Token.Bool(b) =>
        stack = Value.B(b) :: stack
      case Token.Op("+") =>
        applyStack(Expr.Plus.apply)
      case Token.Op("&&") =>
        applyStack(Expr.And.apply)
      case Token.Op(">") =>
        applyStack(Expr.GreaterThan.apply)
      case Token.Op(unknown) =>
        throw Error("invalid token " + unknown)
      case Token.Nop => ()

  stack

