package me
package modlang

import adt_interpreter.*

def adt_demo() =

  def int(v : Int) = Expr.Constant(Value.I(v))
  def bool(v : Boolean) = Expr.Constant(Value.B(v))

  import Expr.*

  run(Plus(int(10), int(5)))
  run(And(GreaterThan(int(10), int(5)), GreaterThan(int(3), int(2))))
  run(And(bool(true), bool(false)))

  val comp = Expr.And(Expr.GreaterThan(Expr.Constant(Value.I(10)), Expr.Constant(Value.I(5))),
    Expr.GreaterThan(Expr.Constant(Value.I(10)), Expr.Constant(Value.I(5))))
  run(comp)
  run(Expr.Plus(Expr.Constant(Value.I(10)), Expr.Constant(Value.I(20))))

enum Token:
  case Number(value: Int)
  case Op(name : String)

def toToken(str : String) : Token =
  str.toIntOption match
    case Some(i) => Token.Number(i)
    case None => Token.Op(str)

def andreMode() =
  // import scala.io.StdIn.readLine
  // val source = readLine()
  val source = "1  2 5 + +"
  stackRun(source)

def stackRun(source : String) =
  val stringTokens = source.split(" +") match
    case l: Array[String|Null] =>
      l.map(x => x match
        case x : String => x
        case _ => throw Error("null token"))
    case _ => throw Error("lexer error")
  val tokens = stringTokens.map(toToken)

  var stack : List[Value] = List()

  for (token <- tokens)
    println(s"read $token")
    token match
      case Token.Number(i) =>
        stack = Value.I(i) :: stack
      case Token.Op("+") =>
        stack match
          case lhs :: rhs :: rem =>
            stack = interprete(Expr.Plus(Expr.Constant(lhs), Expr.Constant(rhs))) :: rem
          case _ =>
            throw Error("stack underflow" + stack)
      case Token.Op(unknown) =>
        throw Error("invalid token " + unknown)

  println("Stack:")
  stack.foreach(println)

@main def Main(args: String*): Unit =
  println("─" * 100)
  println("hello nix")

  adt_demo()
  andreMode()

  println("─" * 100)
