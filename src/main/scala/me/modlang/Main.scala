package me
package modlang

def adt_demo() =
  println("Running adt interpreter")
  import adt_interpreter.*

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

def union_demo() =
  println("Running union interpreter")
  import union_interpreter.*
  def int(v : Int) = Expr(IntValue(v))
  def bool(v : Boolean) = Expr(BoolValue(v))

  run(Expr(Plus(int(10), int(5))))
  run(Expr(And(Expr(GreaterThan(int(10), int(5))), Expr(GreaterThan(int(3), int(2))))))
  run(Expr(And(bool(true), bool(false))))

  val comp = Expr(And(Expr(GreaterThan(int(10), int(5))), Expr(GreaterThan(int(10), int(5)))))
  run(comp)
  run(Expr(Plus(int(10), int(20))))

def andreMode() =
  import stackvm.*
  println("Modular language proto services started")
  runSource("1  2 5 + +")
  runSource("10 2 + 11 > true &&")
  // repl()

@main def Main(args: String*): Unit =
  println("─" * 100)
  println("hello nix")

  adt_demo()
  union_demo()
  andreMode()

  println("─" * 100)
