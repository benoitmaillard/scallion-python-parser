package spp.lexer

import spp.utils._
import spp.structure.Tokens._

import java.io.File
import scala.io.Source
import scala.annotation.tailrec

import sl.Lexers
import sl.Expressions._

import scala.language.implicitConversions

/**
  * Takes an list of files as input and produces the corresponding sequence
  * of tokens
  */
object Lexer extends Lexers {
  type Token = spp.structure.Tokens.Token
  type Value = (List[Int], Int)

  /**
    * Returns an iterator with the resulting tokens from the given files
    *
    * @param ctx context for compilation
    * @param sources list of file
    * @return resulting tokens
    */
  def apply(path: String): Iterator[Token] = {
    val file = new File(path)
    val res = tokenize(path)
    tokenize(path).get.map {
      case Positioned(token, pos) => token.setPos(SourcePosition(file, pos.line, pos.column))
    }.iterator
  }

  def tokenize(path: String) = lexer.tokenizeFromFile(path)

  def unapply(tokens: Seq[Token]): String = {
    def reorder(tokens: Seq[Token], acc: Seq[Token]): Seq[Token] = tokens match {
      case Nil => acc
      case head +: Nil => acc :+ head
      case head +: scnd +: tail => (head, scnd) match {
        case (Newline(), Indent()) => reorder(tail, acc :+ Indent() :+ Newline())
        case (Newline(), Dedent()) => reorder(tail, acc :+ Dedent() :+ Newline())
        case _=> reorder(scnd +: tail, acc :+ head)
      }
    }

    val reordred = reorder(tokens, Seq())


    // code strongly inspired from scallion lambda example
    // https://github.com/epfl-lara/scallion/blob/master/example/lambda/Lambda.scala

    val spaceMap: ((Token, Token)) => String = {
      case (Delimiter(d), _) => if (d.contains("=") || ":;,".contains(d)) " " else ""
      case (_, Delimiter(d)) => if (d.contains("=")) " " else ""
      case (_:Identifier | _:BytesLiteral | _:FloatLiteral | _:ImaginaryLiteral |
        _:IntLiteral | _:StringLiteral | _:Keyword, follow) => follow match {
        case _:Delimiter => ""
        case _ => " "
      }
      case (_:Operator, _) | (_, _:Operator) => " "
      case _ => ""
    }

    val spaces = "" +: reordred.zip(reordred.tail).map(spaceMap)

    val strings = reordred.foldLeft((Seq.empty[String], 0)){
      case ((strings, level), current) => current match {
      case BytesLiteral(prefix, delimiter, value) => (("b\"" ++ value ++ "\"") +: strings, level)
      case Delimiter(del) => (del +: strings, level)
      case Identifier(name) => (name +: strings, level)
      case ImaginaryLiteral(value) => ((value.toString + "j") +: strings, level)
      case FloatLiteral(value) => (value.toString +: strings, level)
      case IntLiteral(value) => (value.toString +: strings, level)
      case StringLiteral(prefix, delimiter, value) => ((prefix + "\"" + value + "\"") +: strings, level)
      case Keyword(name) => (name +: strings, level)
      case Indent() => ("" +: strings, level + 1)
      case Dedent() => ("\n" +: strings, level - 1)
      case Newline() => (("\n" + (" " * 4 * level)) +: strings, level)
      case Operator(op) => (op +: strings, level)
      case EOF() => ("" +: strings, level)
      case _ => ("" +: strings, level)
      }
    }._1.reverse

    spaces.zip(strings).map(x => x._1 + x._2).mkString("")
  }

  def digits(str: String) = str.toSeq.filter(_ != '_').mkString
  
  def parseBigInt(seq: Seq[Char], base: Int): BigInt = {
    seq.filter(_ != '_').reverse.zipWithIndex.foldLeft(BigInt(0)){
      case (bg, (char, i)) =>
      bg + BigInt(base).pow(i) * Integer.parseInt(char.toString, base)
    }
  }

  val space = unit("""\s""") |> {(value, str, pos) => (value, Nil)}

  val keywords = oneOf(
    "False", "None", "True", "and", "as", "assert", "async",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield"
  ) |> { (value, str, pos) => (value, List(Positioned(Keyword(str), pos))) }

  val operators = oneOfEscaped(
    "+", "-", "**", "*", "//", "/", "%", "<<", ">>", "&", "|", 
    "^", "~", ":=", "<=", ">=", "<", ">", "==", "!=", "@"
  ) |> { (value, str, pos) => (value, List(Positioned(Operator(str), pos))) }

  val delimiters = oneOfEscaped(
    ",", ":", ".", ";", "=", "->",
    "+=", "-=", "*=", "/=", "//=", "%=", "@=", "&=", "|=", "^=", ">>=",
    "<<=", "**=",
  ) |> { (value, str, pos) => (value, List(Positioned(Delimiter(str), pos))) }

  val openingDelimiters = oneOfEscaped("(", "[", "{") |>
    { case ((stack, pLevel), str, pos) => ((stack, pLevel + 1), List(Positioned(Delimiter(str), pos))) }
  val closingDelimiters = oneOfEscaped(")", "]", "}") |>
    { case ((stack, pLevel), str, pos) => ((stack, pLevel - 1), List(Positioned(Delimiter(str), pos))) }

  val identifiers = unit("""[a-zA-Z_][a-zA-Z_\d]*""".r) |>
    { (value, str, pos) => (value, List(Positioned(Identifier(str), pos))) }

  // either only zeroes or 1-9 followed by 0-9 (with some _ in between)
  val decimalIntLit = oneOfRe("""0(?:_?0)*""".r, """[1-9](?:_?\d)*""".r) |> 
    { (value, str, pos) => (value, List(Positioned(IntLiteral(BigInt(digits(str))), pos))) }

  val binaryIntLit = "0" ~/~ "[bB]" ~/~ """(?:_?[01])*""" |>
    { case (value, _ ~ _ ~ digits, pos) => (value, List(Positioned(IntLiteral(parseBigInt(digits, 2)), pos))) }

  val octIntLit = "0" ~/~ "[oO]" ~/~ """(?:_?[0-7])*""" |>
    { case (value, _ ~ _ ~ digits, pos) => (value, List(Positioned(IntLiteral(parseBigInt(digits, 8)), pos))) }

  val hexIntLit = "0" ~/~ "[xX]" ~/~ """(?:_?[0-9a-fA-F])*""" |>
    { case (value, _ ~ _ ~ digits, pos) => (value, List(Positioned(IntLiteral(parseBigInt(digits, 16)), pos))) }

  val digitPart = "[0-9]" ~ many("_?" ~ "[0-9]")
  val fraction = """\.""" ~ digitPart 
  val pointFloat = (opt(digitPart) ~ fraction) | (digitPart ~ """\.""")
  val exponent = """[eE][\+\-]?""" ~ digitPart
  val exponentFloat = (digitPart | pointFloat) ~ exponent

  val floatLiteral = ((pointFloat ~ opt(exponent)) | (digitPart ~ exponent)) |> {
    case (value, floatStr, pos) => 
      (value, List(Positioned(FloatLiteral(digits(floatStr).toDouble), pos)))
  }

  val imaginaryLiteral = ((pointFloat ~ opt(exponent)) | (digitPart ~ exponent) | digitPart) ~/~ "[jJ]" |> {
    case (value, floatStr ~ _, pos) =>
      (value, List(Positioned(ImaginaryLiteral(digits(floatStr).toDouble), pos)))
  }

  val stringPrefix = oneOf("RF", "Rf", "rF", "rf", "FR", "fR", "Fr", "fr", "F", "f", "U", "R", "u", "r", "")
  val bytesPrefix = oneOf("RB", "Rb", "rB", "rb", "BR", "bR", "Br", "br", "B", "b")

  def longString(delimiter: Char, isBytes: Boolean = false) = {
    val delimiterFull = delimiter.toString * 3

    val (prefix, isValidChar) = if (isBytes) (bytesPrefix, """[\x00-\x7F]""")  else (stringPrefix, """\p{all}""")

    val re = prefix ~/~ delimiterFull ~/~
      many(raw"""[^\\$delimiter]""" | raw"""\\$isValidChar""" | raw"$delimiter(?!$delimiter$delimiter)") ~/~
      delimiterFull

    re |> {
      case (value, pre ~ _ ~ content ~ _, pos) => (value, List(Positioned(if (isBytes) BytesLiteral(pre, delimiterFull, content) else StringLiteral(pre, delimiterFull, content), pos)))
    }
  }

  def shortString(delimiter: Char, isBytes: Boolean = false) = {
    val (prefix, isValidChar) = if (isBytes) (bytesPrefix, """[\x00-\x7F]""")  else (stringPrefix, """\p{all}""")

    val re = prefix ~/~ delimiter.toString ~/~
      many(raw"""[^\\${'\n'}$delimiter]""" | raw"""\\$isValidChar""") ~/~
      delimiter.toString
    re |> {
      case (value, pre ~ _ ~ content ~ _, pos) => (value, List(Positioned(if (isBytes) BytesLiteral(pre, delimiter.toString, content) else StringLiteral(pre, delimiter.toString, content), pos)))
    }
  }

  val longStringDq = longString('"')
  val longStringSq = longString('\'')
  val longBytesDq = longString('"', true)
  val longBytesSq = longString('"', true)
  val shortStringDq = shortString('"')
  val shortStringSq = shortString('\'')
  val shortBytesDq = shortString('"', true)
  val shortBytesSq = shortString('"', true)

  val physicalNewLine = oneOf("\n", "\r\n", "\r")
  val commentR = "#[^\n\r]*"

  val explicitLineJoin = escape("\\") ~ "\n" |> { (value, _, _) => (value, List()) }

  val indentation = many(" ") ~ opt(commentR) ~          // end of the current line
    many(physicalNewLine ~ many("[ ]") ~ opt(commentR)) ~  // any number of empty lines
    physicalNewLine ~/~ many(" ") |> {
      case ((stack, pLevel), blank ~ indent, pos) =>
        if (pLevel > 0) ((stack, pLevel), List())         // indentation is ignored inside enclosing delimiters
        else if (pos.index == 0)
          if (indent.length > 0) ((stack, pLevel), List(Positioned(ErrorToken("Indentation error"), pos)))
          else ((stack, pLevel), List())
        else stack match {
          case current :: tl =>
            if (indent.length > current)
              ((indent.length :: stack, pLevel), List(Positioned(Newline(), pos), Positioned(Indent(), pos)))
            else {
              val updatedStack = stack.dropWhile(indent.length < _)
              if (indent.length == updatedStack.head) {
                val newLine = Positioned(Newline(), pos)
                val dedents = List.fill(stack.length - updatedStack.length)(Positioned(Dedent(), pos))
                ((updatedStack, pLevel), newLine :: dedents)
              } else ((stack, pLevel), List(Positioned(ErrorToken("Indentation error"), pos)))
            }
        }

    }
  
  // all blanks from the end of the last statement to the end of the file are ignored
  val eof = many("[ \t]") ~ opt(commentR) ~ // end of the current line
    many(physicalNewLine ~ many("[ \t]") ~ opt(commentR)) ~ // any number of empty lines
    "$" |> { (value, _, _) => (value, List()) }
  
  
  val stdRuleSet = RuleSet(
    eof, longStringDq, longStringSq, longBytesDq, longBytesSq, shortStringDq, shortStringSq, shortBytesDq, shortBytesSq, explicitLineJoin, binaryIntLit, octIntLit, hexIntLit, imaginaryLiteral, floatLiteral, decimalIntLit, keywords, operators, delimiters, openingDelimiters, closingDelimiters, identifiers,
    indentation,
    space
  ) withFinalAction {
    case ((stack, pLevel), pos) =>
      if (pos.index == 0) List(Positioned(EOF(), pos)) // TODO condition should be (nTokens == 0)
      else {
        val newLine = List(Positioned(Newline(), pos))
        val dedents = List.fill(stack.length - 1)(Positioned(Dedent(), pos))
        val eof = List(Positioned(EOF(), pos))

        newLine ::: dedents ::: eof
      }
  }

  lazy val lexer: Lexer = Lexer(LexerState(stdRuleSet, (List(0), 0)), ErrorToken("Unexpected token"))
  
}

/**
  * Displays each tokens on a separated line
  */
object TokensPrinter {
  def apply(tokens: Iterator[Token]) = {
    tokens.foreach(println(_))
  }
}