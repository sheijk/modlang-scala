package me
package modlang
package tfi

package Empty:
  trait Lang[T]:
    type Expr
    type Result = T
    def eval(e: Expr): Result

trait EvalId[T] extends Empty.Lang[T]:
  type Expr = Result
  def eval(e: Expr) = e

trait EvalFn[T] extends Empty.Lang[T]:
  type Expr = () => T
  type Result = T
  def eval(e: Expr) = e()

  def fromInt(v: Int) = () => v
  def asInt(t: Expr) = t().asInstanceOf[Int]
  def fromBool(v: Boolean) = () => v
  def asBool(t: Expr) = t().asInstanceOf[Boolean]

trait EvalInt extends Empty.Lang[Int]:
  def fromInt(v: Int) = v
  def asInt(v: Expr) = v

trait EvalBool extends EvalId[Boolean]:
  def fromBool(v: Boolean): Expr = v
  def asBool(t: Expr): Boolean = t

trait EvalIntBool[V >: Int|Boolean] extends Empty.Lang[V]:
  type Expr = V
  def fromBool(v: Boolean): V = v
  def asBool(t: Expr): Boolean = t.asInstanceOf[Boolean]
  def fromInt(v: Int): Expr = v
  def asInt(t: Expr): Int = t.asInstanceOf[Int]

