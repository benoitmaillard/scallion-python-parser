package spp.lexer

import spp.utils._
import spp.structure.Tokens._
import spp.lexer.TokensCleaner._

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
      case BytesLiteral(value) => (("b\"" ++ value ++ "\"") +: strings, level)
      case Delimiter(del) => (del +: strings, level)
      case Identifier(name) => (name +: strings, level)
      case ImaginaryLiteral(value) => ((value.toString + "j") +: strings, level)
      case FloatLiteral(value) => (value.toString +: strings, level)
      case IntLiteral(value) => (value.toString +: strings, level)
      case StringLiteral(prefix, value) => ((prefix.getOrElse("") + "\"" + value + "\"") +: strings, level)
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
  
  def removeDelimiters(str: String, delimiter: Char) = {
    val nDelimiters = str.indexWhere(_ != delimiter)
    str.drop(nDelimiters).dropRight(nDelimiters)
  }
  
  // Splits a raw literal into prefix and value, removes delimiters
  def makeStringLiteral(input: String): StringLiteral = {
    val delimiter = input.find(c => c == '"' || c == '\'').get
    val prefixLength = input.indexOf(delimiter)
    
    val (prefix, rightPart) = input.splitAt(prefixLength)
    val value = removeDelimiters(rightPart, delimiter)
    
    if (prefixLength > 0) StringLiteral(Some(prefix), value)
    else StringLiteral(None, value)
  }

  def digits(str: String) = str.toSeq.filter(_ != '_').mkString
  
  def parseBigInt(seq: Seq[Char], base: Int): BigInt = {
    seq.filter(_ != '_').reverse.zipWithIndex.foldLeft(BigInt(0)){
      case (bg, (char, i)) =>
      bg + BigInt(base).pow(i) * Integer.parseInt(char.toString, base)
    }
  }

  val space = unit("""\W""") |> {(value, str, pos) => (value, Nil)}

  val keywords = oneOf(
    "False", "None", "True", "and", "as", "assert", "async",
    "await", "break", "class", "continue", "def", "del", "elif",
    "else", "except", "finally", "for", "from", "global", "if",
    "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield"
  ) |> { (value, str, pos) => (value, List(Positioned(Keyword(str), pos))) }

  val operators = oneOfEscaped(
    "+", "-", "*", "**", "/", "//", "%", "@", "<<", ">>", "&",
    "|", "^", "~", ":=", "<", ">", "<=", ">=", "==", "!="
  ) |> { (value, str, pos) => (value, List(Positioned(Operator(str), pos))) }

  val delimiters = oneOfEscaped(
    ",", ":", ".", ";", "@", "=",
    "->", "+=", "-=", "*=", "/=", "//", "%=", "@=", "&=", "|=",
    "^=", ">>", "<<", "**="
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

  println((pointFloat | exponentFloat).build())

  val floatLiteral = ((pointFloat ~ opt(exponent)) | (digitPart ~ exponent)) |> {
    case (value, floatStr, pos) => 
      (value, List(Positioned(FloatLiteral(digits(floatStr).toFloat), pos)))
  }

  val imaginaryLiteral = ((pointFloat ~ opt(exponent)) | (digitPart ~ exponent) | digitPart) ~/~ "[jJ]" |> {
    case (value, floatStr ~ _, pos) =>
      (value, List(Positioned(ImaginaryLiteral(digits(floatStr).toFloat), pos)))
  }

  val physicalNewLine = oneOf("\n", "\r\n", "\r")
  val commentR = "#[^\n\r]*"

  val indentation = many(" ") ~ opt(commentR) ~          // end of the current line
    many(physicalNewLine ~ many("[ ]") ~ opt(commentR)) ~  // any number of empty lines
    physicalNewLine ~/~ many(" ") |> {
      case ((stack, pLevel), blank ~ indent, pos) =>
        if (pLevel > 0) ((stack, pLevel), List())         // indentation is ignored inside enclosing delimiters
        else if (pos.index == 0)
          if (indent.length > 0) throw new LexerError("Indentation error", pos)
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
              } else throw new LexerError("Indentation error", pos)
            }
        }

    }
  
  // all blanks from the end of the last statement to the end of the file are ignored
  val eof = many("[ \t]") ~ opt(commentR) ~ // end of the current line
    many(physicalNewLine ~ many("[ \t]") ~ opt(commentR)) ~ // any number of empty lines
    "$" |> { (value, _, _) => (value, List()) }
  
  
  val stdRuleSet = RuleSet(
    eof, binaryIntLit, octIntLit, hexIntLit, imaginaryLiteral, floatLiteral, decimalIntLit, keywords, operators, delimiters, openingDelimiters, closingDelimiters, identifiers,
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

  lazy val lexer: Lexer = Lexer(LexerState(stdRuleSet, (List(0), 0)))
  
}

/**
  * Displays each tokens on a separated line
  */
object TokensPrinter {
  def apply(tokens: Iterator[Token]) = {
    tokens.foreach(println(_))
  }
}