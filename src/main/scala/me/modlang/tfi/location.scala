package me
package modlang
package tfi

case class Location(file: String, line: Int, column: Int):
  override def toString() =
    s"$file:$line:$column"

import scala.quoted.*
import scala.language.experimental.macros

trait LocationMacro:
  inline implicit def generate: Location = ${ locationImpl() }

def locationImpl()(using scala.quoted.Quotes): scala.quoted.Expr[Location] =
  import scala.quoted.quotes.reflect.*
  val pos = Position.ofMacroExpansion
  val path = pos.sourceFile.jpath.toString
  val startLine = pos.startLine + 1
  '{ new Location(${ Expr(path) }, ${ Expr(startLine) }, 0) }

object Location extends LocationMacro:
  def empty: Location = new Location("", 0, 0)

package CaptureLocation:
  def f[T1,T2](a: T1, b: T2)(implicit loc : Location) = (a, b, loc)

