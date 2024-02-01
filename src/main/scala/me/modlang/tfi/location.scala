package me
package modlang
package tfi

// import scala.reflect.macros.blackbox.Context
// 
// def locationImpl(c: Context): c.Tree = {
//   import c.universe._
//   val line = Literal(Constant(c.enclosingPosition.line))
//   val path = Literal(Constant(c.enclosingPosition.source.path))
//   New(c.mirror.staticClass(classOf[Location].getName()), path, line)
// }

case class Location(file: String, line: Int, column: Int):
  override def toString() =
    s"$file:$line:$column"

import scala.quoted.*
import scala.language.experimental.macros

trait LocationMacro:
  inline implicit def generate: Location = ${ locationImpl() }
  // implicit def generate: Location = macro locationImpl

def locationImpl()(using scala.quoted.Quotes): scala.quoted.Expr[Location] =
  import scala.quoted.quotes.reflect.*
  val pos = Position.ofMacroExpansion
  val path = pos.sourceFile.jpath.toString
  val startLine = pos.startLine + 1
  // Location(path, startLine, pos.startColumn )
  '{ new Location(${ Expr(path) }, ${ Expr(startLine) }, 0) }
  // '{ Location("foo.scala", 0, 0) }

// import scala.language.experimental.macros

object Location extends LocationMacro:
  def empty: Location = new Location("", 0, 0)
// given Location()

package CaptureLocation:
  def f[T1,T2](a: T1, b: T2)(implicit loc : Location) = (a, b, loc)

