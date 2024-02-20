package me
package modlang
package tc

// -----------------------------------------------------------------------------
// Types

package Empty:
  trait Lang[T]:
    type Expr
    type Result = T
    def eval(e: Expr): T

type Eval[T] = () => T

// -----------------------------------------------------------------------------
// Mixins

// Implements Lang[String] as this is only used in ToString for now
trait EvalId[T] extends Empty.Lang[T]:
  type Expr = Result
  def eval(e: Expr) = e

trait EvalFn[T] extends Empty.Lang[T]:
  type Expr = () => T
  type Result = T // remove
  def eval(e: Expr) = e()

transparent trait EvalHasBool[T]:
  def fromBool(v: Boolean): T
  def asBool(t: T): Boolean

transparent trait EvalHasInt[T]:
  def fromInt(v: Int): T
  def asInt(t: T): Int

trait EvalInt[T >: Int] extends EvalHasInt[T]:
  def fromInt(v: Int): T = v
  def asInt(v: T) = v.asInstanceOf[Int]

trait EvalBool[T >: Boolean] extends EvalHasBool[T]:
  def fromBool(v: Boolean): T = v
  def asBool(t: T): Boolean = t.asInstanceOf[Boolean]

trait EvalIntBool[T >: Int|Boolean] extends EvalBool[T], EvalInt[T]

