package me
package modlang
package stackvm

enum Token:
  case Number(value: Int)
  case Op(name : String)

def toToken(str : String) : Token =
  str.toIntOption match
    case Some(i) => Token.Number(i)
    case None => Token.Op(str)

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
  println("Stack:")
  stack.foreach(println)

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

  stack

