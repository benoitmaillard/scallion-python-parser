package spp.lexer

import spp.utils._
import spp.structure.Tokens._
import spp.lexer.TokensCleaner._

import java.io.File
import scallion.lexical._
import scallion.input._
import scala.annotation.tailrec

/**
  * Takes an list of files as input and produces the corresponding sequence
  * of tokens
  */
object Lexer extends Lexers with CharRegExps {
  type Token = spp.structure.Tokens.Token
  type Position = SourcePosition

  /**
    * Returns an iterator with the resulting tokens from the given files
    *
    * @param ctx context for compilation
    * @param sources list of file
    * @return resulting tokens
    */
  def apply(ctx: Context, sources: File): Iterator[Token] = {
    val tokens = lexer.spawn(
      Source.fromFile(sources, SourcePositioner(sources))
    ).toList

    val result = TokensCleaner(tokens)(ctx).process()
    result.iterator
  }

  def unapply(tokens: Seq[Token]): String = {
    val strings = tokens map {
      case BytesLiteral(value) => "b\"" + value + "\""
      case Dedent() => ""
      case Delimiter(del) => del
      case EOF() => ""
      case FloatLiteral(value) => value
      case Identifier(name) => name
      case ImaginaryLiteral(value) => value.toString + "j" 
      case Indent() => ""
      case IntLiteral(value) => value
      case Keyword(name) => name
      case Newline() => "\n"
      case Operator(op) => op
      case StringLiteral(prefix, value) => prefix + "\"" + value + "\""
      case _ => ""
    }

    strings.mkString(" ")
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
  
  def parseBigInt(seq: Seq[Char], base: Int): BigInt = {
    // remove prefix (ex: 0b) and grouping characters
    def clean(seq: Seq[Char]) = seq.drop(2).filter(_ != '_')
    
    clean(seq).reverse.zipWithIndex.foldLeft(BigInt(0)){
      case (bg, (char, i)) =>
      bg + BigInt(base).pow(i) * Integer.parseInt(char.toString, base)
    }
  }
  
  def not(exluded: Char*) = elem(!exluded.contains(_))

  def oneOfWords(words: String*) = {
    words.map(word(_)).reduce(_ | _)
  }
  
  // identifiers
  val idStart = elem(_.isLetter) | elem('_')
  val idContinue = idStart | elem(_.isDigit)

  // string literals
  val stringPrefix = oneOfWords(
    "r", "u", "R", "U", "f", "F", "fr", "Fr", "fR", "FR",
    "rf", "rF", "Rf", "RF"
  )
  val escapedChar = elem('\\') ~ any // any should be "ascii"
  val shortString =
    elem('\'') ~ many(elem(!"\\\'\n".contains(_)) | escapedChar) ~ elem('\'') |
    elem('\"') ~ many(elem(!"\\\"\n".contains(_)) | escapedChar) ~ elem('\"')
  val longString =
    word("\"" * 6) | word("'" * 6) |
    /* the string literal stops as soon as we get 3 consecutive " or ', last character of a long string
    * cannot be the same character as the delimiter */
    word("\"" * 3) ~ many(not('\\') | escapedChar) ~ (not('\\', '"') | escapedChar) ~ word("\"" * 3) |
    word("'" * 3) ~ many(not('\\') | escapedChar) ~ (not('\\', '\'') | escapedChar) ~ word("'" * 3)
  
  // floating point literals
  val floatDigitPart = digit ~ many(opt(elem('_')) ~ digit)
  val pointFloat = opt(floatDigitPart) ~ elem('.') ~ floatDigitPart | floatDigitPart ~ elem('.')
  val exponentFloat = (floatDigitPart | pointFloat) ~ oneOf("eE") ~ opt(oneOf("+-")) ~ floatDigitPart
  
  // indentation
  val physicalNewLine = oneOfWords("\n", "\r\n", "\r")
  val noLineBreak = elem(c => c != '\n' && c != '\r')
  val spaceChar = elem(_.isSpaceChar)

  val comment = elem('#') ~ many(noLineBreak)
  
  val lexer: Lexer = Lexer(
    // counting indentation spaces, used to generate INDENT/DEDENT/NEWLINE later
    /* there can be any number of blank lines (only spaces or comments) before an
    actual new logical line */
    many(spaceChar) ~ opt(comment) ~
    many(physicalNewLine ~ many(spaceChar) ~ opt(comment)) ~
      physicalNewLine ~ many(whiteSpace) |>
      {(s, range) =>
        val nIndent = s.reverse.indexWhere(c => c == '\n' || c == '\r')
        PhysicalIndent(nIndent).setPos(range._1)
      },

    // spaces that are not placed after a linebreak have no particular meaning
    many(elem(_.isSpaceChar)) |> {(s, range) => Space().setPos(range._1)},

    // explicit line joining with '\'
    elem('\\') ~ many(noLineBreak) ~ physicalNewLine |>
      {(s, range) =>
        // we make sure that the line break is directly after the '\'
        if (s(1) == '\r' || s(1) == '\n')
          Space().setPos(range._1)
        else
          ErrorToken("unexpected character after line continuation character")
      },

    // comment
    elem('#') ~ many(elem(_ != '\n')) |> {(s, range) => Comment().setPos(range._1)},

    
    
    // keywords
    oneOfWords(
      "False", "None", "True", "and", "as", "assert", "async",
      "await", "break", "class", "continue", "def", "del", "elif",
      "else", "except", "finally", "for", "from", "global", "if",
      "import", "in", "is", "lambda", "nonlocal", "not", "or",
      "pass", "raise", "return", "try", "while", "with", "yield"
    ) |>
      {(s, range) => Keyword(s.mkString).setPos(range._1)},

    // operators
    oneOfWords(
      "+", "-", "*", "**", "/", "//", "%", "@", "<<", ">>", "&",
      "|", "^", "~", ":=", "<", ">", "<=", ">=", "==", "!="
    ) |> {(s, range) => Operator(s.mkString).setPos(range._1)},

    // delimiters
    oneOfWords(
      "(", ")", "[", "]", "{", "}", ",", ":", ".", ";", "@", "=",
      "->", "+=", "-=", "*=", "/=", "//", "%=", "@=", "&=", "|=",
      "^=", ">>", "<<", "**="
    ) |> {(s, range) => Delimiter(s.mkString).setPos(range._1)},

    // identifiers
    idStart ~ many(idContinue) |>
      {(s, range) => Identifier(s.mkString).setPos(range._1)},
    
    // string literals
    opt(stringPrefix) ~ (shortString | longString) |>
      {(s, range) => makeStringLiteral(s.mkString).setPos(range._1)},
    
    // integer literals (decimal, binary, octal, hex)
    nonZero ~ many(opt(elem('_')) ~ digit) | many1(elem('0')) ~ many(opt(elem('_')) ~ elem('0')) |>
      {(s, range) => IntLiteral(BigInt(s.filter(_ != '_').mkString)).setPos(range._1)},
    elem('0') ~ oneOf("bB") ~ many1(opt(elem('_')) ~ oneOf("01")) |>
      {(s, range) => IntLiteral(parseBigInt(s, 2)).setPos(range._1)},
    elem('0') ~ oneOf("oO") ~ many1(opt(elem('_')) ~ elem(c => c >= '0' && c <= '7')) |>
      {(s, range) => IntLiteral(parseBigInt(s, 8)).setPos(range._1)},
    elem('0') ~ oneOf("xX") ~ many1(opt(elem('_')) ~ hex) |>
      {(s, range) => IntLiteral(parseBigInt(s, 16)).setPos(range._1)},
    
    // floating point literals
    pointFloat | exponentFloat |>
      {(s, range) => FloatLiteral(s.filter(_ != '_').mkString.toFloat)},

    // imaginary literals
    (pointFloat | exponentFloat | floatDigitPart) ~ oneOf("jJ") |>
      {(s, range) => ImaginaryLiteral(s.filter(_ != '_').mkString.dropRight(1).toFloat)},
  ) onError {
    (cs, range) => ErrorToken(cs.mkString).setPos(range._1)
  } onEnd {
    pos => EOF().setPos(pos)
  }
}

/**
  * Displays each tokens on a separated line
  */
object TokensPrinter {
  def apply(tokens: Iterator[Token]) = {
    tokens.foreach(println(_))
  }
}