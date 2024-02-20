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

  transparent trait Nested[T, Inner <: Lang[T]](inner_ : Inner) extends Lang[T]:
    val inner = inner_
    def toOuter(e: inner.Expr): Expr
    def toInner(e: Expr): inner.Expr

    override def eval(e: Expr): Result =
      inner.eval(toInner(e))

  trait Dup[T, L <: Lang[T]](val left : L, val right : L, mergeLangs: (T, T) => T) extends Lang[T]:
    type Expr = (left.Expr, right.Expr)
    override def eval(e: Expr) =
      mergeLangs(left.eval(e._1), right.eval(e._2))

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

