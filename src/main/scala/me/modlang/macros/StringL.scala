package me
package modlang
package macro_compiler

package StringL:
  import tfi.{Calc_int, Calc_bool, Calc}
  import Calc_int.{given}
  import Calc_bool.{given}
  import Calc.{given}

  trait ParseLang[T, L[T] <: tfi.Empty.Lang[T]](val lang: L[T]):
    type Parser = SymEx => Option[lang.Expr]
    type SymbolTable = Scope[Parser]
    def rules(): List[Parser] = List()
    def parseString(str: String): Option[lang.Expr]

    def rewriteRule(pattern: SymEx, replace: Map[String, lang.Expr] => lang.Expr): SymEx => Option[lang.Expr] =
      ex =>
        ex.bindIdsInPattern(pattern) match
        case Right(replacements) =>
          val replTs = replacements.map((name, repl) => (name, parse(repl).getOrElse(throw Error("foo")))).toMap
          Some(replace(replTs))
        case Left(_) => None

    def rewriteRule(pattern: String, replace: Map[String, lang.Expr] => lang.Expr): SymEx => Option[lang.Expr] =
      val patternEx = SymEx.parse(pattern)
      patternEx.map(rewriteRule(_, replace)).getOrElse(throw Error(s"invalid pattern: $pattern"))

    def parse(ex: SymEx): Option[lang.Expr] =
      def step(rules: List[Parser]): Option[lang.Expr] =
        rules match
        case r :: rem => r(ex).orElse(step(rem))
        case Nil => None
      step(rules()).orElse(ex.id().flatMap(parseString))

  extension[T, L[T] <: tfi.Calc_int.Lang[T]] (p: ParseLang[T, L])
    def calcIntRules() =
      List(p.rewriteRule("(plus $lhs $rhs)", args => p.lang.plus(args("$lhs"), args("$rhs"))))

  def parseIntValue[T](using lang: tfi.Calc_int.Lang[T])(str: String): Option[lang.Expr] =
      str.toIntOption.map(lang.int)

  given ParseLang[Int, Calc_int.Lang](summon[Calc_int.Lang[Int]]) with
    override def rules() = this.calcIntRules()
    def parseString(str: String): Option[lang.Expr] = parseIntValue(using lang)(str)

  extension[T, L[T] <: tfi.Calc_bool.Lang[T]] (p: ParseLang[T, L])
    def calcBoolRules() =
      List(p.rewriteRule("(and $lhs $rhs)", args => p.lang.and(args("$lhs"), args("$rhs"))))

  def parseBoolValue[T](using lang: tfi.Calc_bool.Lang[T])(str: String): Option[lang.Expr] =
      if str == "true" then Some(lang.bool(true))
      else if str == "false" then Some(lang.bool(false))
      else None

  given ParseLang[Boolean, Calc_bool.Lang](summon[Calc_bool.Lang[Boolean]]) with
    override def rules() = this.calcBoolRules()
    def parseString(str: String): Option[lang.Expr] = parseBoolValue(using lang)(str)

  extension[T, L[T] <: tfi.Calc.Lang[T]] (p: ParseLang[T, L])
    def calcRules() =
      p.calcIntRules() ++ p.calcBoolRules() ++
      List(p.rewriteRule("(greaterThan $lhs $rhs)", args => p.lang.greaterThan(args("$lhs"), args("$rhs"))))

  def parseCalcValue[T](using lang: tfi.Calc.Lang[T])(str: String): Option[lang.Expr] =
      parseBoolValue(str).orElse(parseIntValue(str))

  given ParseLang[Calc.Value, Calc.Lang](summon[Calc.Lang[Calc.Value]]) with
    def parseString(str: String): Option[lang.Expr] = parseCalcValue(using lang)(str)
    override def rules() = this.calcRules()

  def demo() =
    println("Parsing tfi language")
    import tfi.*
    val intExs = List("10", "plus 10 20", "plus (plus 5 5) 20")
    val boolExs = List("true", "false", "and true false", "and (and true false) true")
    val calcExs = intExs ++ boolExs ++ List("and (greaterThan (plus 1 2) 2) true")
    run[Int, tfi.Calc_int.Lang](intExs*)
    run[Boolean, tfi.Calc_bool.Lang](boolExs*)
    run[Calc.Value, tfi.Calc.Lang](calcExs*)

  def run[T, L[T] <: tfi.Empty.Lang[T]](src: String*)(using eval : L[T], parser : ParseLang[T, L]) =
    def p(src: String) =
      val ex: Option[parser.lang.Expr] = parser.parse(SymEx.parse(src).getOrElse(SymEx.sym("parsing error")))
      ex match
      case Some(ex) =>
        println(s"  eval(${src}) = ${parser.lang.eval(ex)}")
      case None =>
        println(s"  failed to parse $src")
    println("  " + eval.toString)
    src.foreach(p)
