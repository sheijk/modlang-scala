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

def andreMode() =
  import scala.io.StdIn.readLine

  // val source = readLine()
  val tokens = "1  2 5 + +".split(" +") match
    case l: Array[String|Null] =>
      l.map(x => x match
        case x : String => x
        case _ => throw Error("null token"))
    case _ => throw Error("lexer error")

  var stack : List[Value] = List()

  for (token <- tokens)
    println(token)
    token.toIntOption match
      case Some(i) =>
        stack = Value.I(i) :: stack
      case None =>
        token match
          case "+" =>
            stack match
              case lhs :: rhs :: rem =>
                stack = adt_interpreter.interprete(Expr.Plus(Expr.Constant(lhs), Expr.Constant(rhs))) :: rem
              case _ => throw Error("stack underflow" + stack)
          case unknown =>
            throw Error("invalid token " + unknown)

  println("Stack:")
  stack.foreach(println)

@main def Main(args: String*): Unit =
  println("─" * 100)
  println("hello nix")

  adt_demo()
  andreMode()

  println("─" * 100)
