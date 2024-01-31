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

