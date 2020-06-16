package spp.lexer

import spp.structure.Tokens._

import java.io.File
import scala.io.Source
import scala.annotation.tailrec

import spp.utils.SourcePosition
import spp.utils.StringPosition

import sl.Lexers
import sl.Expressions._
import sl.Rules._
import sl.Positions._

import scala.language.implicitConversions

/**
  * Takes an list of files as input and produces the corresponding sequence
  * of tokens
  */
object Lexer extends Lexers {
  type Token = spp.structure.Tokens.Token
  type Value = (List[Int], Int)

  type LexerRule = Rule[Value, Token]

  /**
    * Returns an iterator with the resulting tokens from the given files
    *
    * @param ctx context for compilation
    * @param sources list of file
    * @return resulting tokens
    */
  def apply(path: String): Iterator[Token] = {
    val file = new File(path)
    val (tokens, finalState) = tokenizeFromFile(path).get

    val tokensFinal: List[WithPosition[Token]] = tokens match {
      case Nil => List(WithPosition(EOF(), Position.initial, Position.initial))
      case _ => {
        val newLine = List[WithPosition[Token]](WithPosition(Newline(), tokens.last.start, tokens.last.end))
        val dedents = List.fill[WithPosition[Token]](finalState._1.length - 1)(WithPosition(Dedent(), tokens.last.start, tokens.last.end))
        val eof = List[WithPosition[Token]](WithPosition(EOF(), tokens.last.start, tokens.last.end))

        tokens ::: newLine ::: dedents ::: eof
      }
    }
    
    tokensFinal.map {
      case WithPosition(token, start, end) => token.setPos(SourcePosition(file, start.line, start.column))
    }.iterator
  }

  def tokenizeFromFile(path: String) = lexer.tokenizeFromFile(path)


  def applyString(input: String): Iterator[Token] = {
    val tokens = tokenizeFromString(input).get._1.filter{
      case WithPosition(Newline(), _, _) => false
      case _ => true
    }
    
    tokens.map {
      case WithPosition(token, start, end) => token.setPos(StringPosition(start.index, input))
    }.iterator
  }

  def tokenizeFromString(input: String) = lexer.tokenizeFromString(input)

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

  val space: LexerRule = unit("""\s""") |> {(value, res) => (value, Nil)}

  val keywords: LexerRule = oneOf(
    "False", "None", "True", "and", "assert", "async", "as",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield"
  ) |> { case (value, List(res)) => (value, List(Keyword(res.str))) }

  val operators: LexerRule = oneOfEscaped(
    "+", "-", "**", "*", "//", "/", "%", "<<", ">>", "&", "|", 
    "^", "~", ":=", "<=", ">=", "<", ">", "==", "!=", "@", "..."
  ) |> { case (value, List(res)) => (value, List(Operator(res.str))) }

  val delimiters: LexerRule = oneOfEscaped(
    ",", ":", ".", ";", "=", "->",
    "+=", "-=", "*=", "/=", "//=", "%=", "@=", "&=", "|=", "^=", ">>=",
    "<<=", "**=", "!"
  ) |> { case (value, List(res)) => (value, List(Delimiter(res.str))) }

  val openingDelimiters: LexerRule = oneOfEscaped("(", "[", "{") |>
    { case ((stack, pLevel), List(res)) => ((stack, pLevel + 1), List(Delimiter(res.str))) }
  val closingDelimiters: LexerRule = oneOfEscaped(")", "]", "}") |>
    { case ((stack, pLevel), List(res)) => ((stack, pLevel - 1), List(Delimiter(res.str))) }

  val identifiers: LexerRule = unit("""[a-zA-Z_][a-zA-Z_\d]*""".r) |>
    { case (value, List(res)) => (value, List(Identifier(res.str))) }

  // either only zeroes or 1-9 followed by 0-9 (with some _ in between)
  val decimalIntLit: LexerRule = oneOfRe("""0(?:_?0)*""".r, """[1-9](?:_?\d)*""".r) |> 
    { case (value, List(res)) => (value, List(IntLiteral(BigInt(digits(res.str))))) }

  val binaryIntLit: LexerRule = "0" ~/~ "[bB]" ~/~ """(?:_?[01])*""" |>
    { case (value, List(_, _, digits)) => (value, List(IntLiteral(parseBigInt(digits.str, 2)))) }

  val octIntLit: LexerRule = "0" ~/~ "[oO]" ~/~ """(?:_?[0-7])*""" |>
    { case (value, List(_, _, digits)) => (value, List(IntLiteral(parseBigInt(digits.str, 8)))) }

  val hexIntLit: LexerRule = "0" ~/~ "[xX]" ~/~ """(?:_?[0-9a-fA-F])*""" |>
    { case (value, List(_, _, digits)) => (value, List(IntLiteral(parseBigInt(digits.str, 16)))) }

  val digitPart = "[0-9]" ~ many("_?" ~ "[0-9]")
  val fraction = """\.""" ~ digitPart 
  val pointFloat = (opt(digitPart) ~ fraction) | (digitPart ~ """\.""")
  val exponent = """[eE][\+\-]?""" ~ digitPart
  val exponentFloat = (digitPart | pointFloat) ~ exponent

  val floatLiteral: LexerRule = ((pointFloat ~ opt(exponent)) | (digitPart ~ exponent)) |> {
    case (value, List(float)) => 
      (value, List(FloatLiteral(digits(float.str).toDouble)))
  }

  val imaginaryLiteral: LexerRule = ((pointFloat ~ opt(exponent)) | (digitPart ~ exponent) | digitPart) ~/~ "[jJ]" |> {
    case (value, List(float, _)) =>
      (value, List(ImaginaryLiteral(digits(float.str).toDouble)))
  }

  val stringPrefix = oneOf("RF", "Rf", "rF", "rf", "FR", "fR", "Fr", "fr", "F", "f", "U", "R", "u", "r", "")
  val bytesPrefix = oneOf("RB", "Rb", "rB", "rb", "BR", "bR", "Br", "br", "B", "b")

  def longString(delimiter: Char, isBytes: Boolean = false): LexerRule = {
    val delimiterFull = delimiter.toString * 3

    val (prefix, isValidChar) = if (isBytes) (bytesPrefix, """[\x00-\x7F]""")  else (stringPrefix, """\p{all}""")

    val re = prefix ~/~ delimiterFull ~/~
      many(raw"""[^\\$delimiter]""" | raw"""\\$isValidChar""" | raw"$delimiter(?!$delimiter$delimiter)") ~/~
      delimiterFull

    re |> {
      case (value, List(pre, _, content, _)) => {
        val prefix = pre.str.toLowerCase
        if (prefix.contains('b')) (value, List(BytesLiteral(prefix, delimiterFull, content.str)))
        else (value, List(StringLiteral(prefix, delimiterFull, content.str)))
      }
    }
  }

  def shortString(delimiter: Char, isBytes: Boolean = false): LexerRule = {
    val (prefix, isValidChar) = if (isBytes) (bytesPrefix, """[\x00-\x7F]""")  else (stringPrefix, """\p{all}""")

    val re = prefix ~/~ delimiter.toString ~/~
      many(raw"""[^\\${'\n'}$delimiter]""" | raw"""\\$isValidChar""") ~/~
      delimiter.toString
    re |> {
      case (value, List(pre, _, content, _)) => {
        val prefix = pre.str.toLowerCase
        if (prefix.contains('b')) (value, List(BytesLiteral(pre.str, delimiter.toString, content.str)))
        else (value, List(StringLiteral(pre.str, delimiter.toString, content.str)))
      }
    }
  }

  val longStringDq = longString('"')
  val longStringSq = longString('\'')
  val longBytesDq = longString('"', true)
  val longBytesSq = longString('\'', true)
  val shortStringDq = shortString('"')
  val shortStringSq = shortString('\'')
  val shortBytesDq = shortString('"', true)
  val shortBytesSq = shortString('\'', true)

  val physicalNewLine = oneOf("\n", "\r\n", "\r")
  val commentR = "#[^\n\r]*"

  val explicitLineJoin: LexerRule = escape("\\") ~ "\n" |> { case (value, _) => (value, List()) }

  val indentation: LexerRule = many(" ") ~ opt(commentR) ~          // end of the current line
    many(physicalNewLine ~ many("[ ]") ~ opt(commentR)) ~  // any number of empty lines
    physicalNewLine ~/~ many(" ") |> {
      case ((stack, pLevel), List(blank, indent)) =>
        if (pLevel > 0) ((stack, pLevel), List())         // indentation is ignored inside enclosing delimiters
        else if (blank.start.index == 0)
          if (indent.str.length > 0) ((stack, pLevel), List(ErrorToken("Indentation error")))
          else ((stack, pLevel), List())
        else stack match {
          case current :: tl =>
            if (indent.str.length > current)
              ((indent.str.length :: stack, pLevel), List(Newline(), Indent()))
            else {
              val updatedStack = stack.dropWhile(indent.str.length < _)
              if (indent.str.length == updatedStack.head) {
                val newLine = Newline()
                ((updatedStack, pLevel), newLine :: List.fill[Positioned[Token]](stack.length - updatedStack.length)(Dedent()))
              } else ((stack, pLevel), List(ErrorToken("Indentation error")))
            }
        }

    }
  
  // all blanks from the end of the last statement to the end of the file are ignored
  val eof: LexerRule = many("[ \t]") ~ opt(commentR) ~ // end of the current line
    many(physicalNewLine ~ many("[ \t]") ~ opt(commentR)) ~ // any number of empty lines
    "$" |> { case (value, _) => (value, List()) }
  
  
  lazy val lexer = Lexer(
    eof, longStringDq, longStringSq, longBytesDq, longBytesSq, shortStringDq, shortStringSq, shortBytesDq,
    shortBytesSq, explicitLineJoin, binaryIntLit, octIntLit, hexIntLit, imaginaryLiteral, floatLiteral,
    decimalIntLit, keywords, operators, delimiters, openingDelimiters, closingDelimiters, identifiers,
    indentation, space
  )((List(0), 0))
}

/**
  * Displays each tokens on a separated line
  */
object TokensPrinter {
  def apply(tokens: Iterator[Token]) = {
    tokens.foreach(println(_))
  }
}