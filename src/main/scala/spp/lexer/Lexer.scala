package spp.lexer

import spp.utils._
import spp.structure.Tokens._

import java.io.File
import scallion.lexical._
import scallion.input._

object Lexer extends Pipeline[List[File], Iterator[Token]] with Lexers
with CharRegExps {
    type Token = spp.structure.Tokens.Token
    type Position = SourcePosition

    def oneOfWords(words: String*) = {
        words.map(word(_)).reduce(_ | _)
    }

    def removeDelimiters(str: String, delimiter: Char) = {
        val nDelimiters = str.indexWhere(_ != delimiter)
        str.drop(nDelimiters).dropRight(nDelimiters)
    }

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

    val escapedChar = elem('\\') ~ any // any should be "ascii"
    
    val keyword = oneOfWords(
        "False", "None", "True", "and", "as", "assert", "async",
        "await", "break", "class", "continue", "def", "del", "elif",
        "else", "except", "finally", "for", "from", "global", "if",
        "import", "in", "is", "lambda", "nonlocal", "not", "or",
        "pass", "raise", "return", "try", "while", "with", "yield"
    )

    val operator = oneOfWords(
        "+", "-", "*", "**", "/", "//", "%", "@", "<<", ">>", "&",
        "|", "^", "~", ":=", "<", ">", "<=", ">=", "==", "!="
    )

    val delimiters = oneOfWords(
        "(", ")", "[", "]", "{", "}", ",", ":", ".", ";", "@", "=",
        "->", "+=", "-=", "*=", "/=", "//", "%=", "@=", "&=", "|=",
        "^=", ">>", "<<", "**="
    )

    val stringPrefix = oneOfWords(
        "r", "u", "R", "U", "f", "F", "fr", "Fr", "fR", "FR",
        "rf", "rF", "Rf", "RF"
    )

    val shortString =
        elem('\'') ~ many(elem(!"\\\'\n".contains(_)) | escapedChar) ~ elem('\'') |
        elem('\"') ~ many(elem(!"\\\"\n".contains(_)) | escapedChar) ~ elem('\"')

    val longString =
        word("\"" * 6) | word("'" * 6) |
        /* the string literal stops as soon as we get 3 consecutive " or ', last character of a long string
         * cannot be the same character as the delimiter */
        word("\"" * 3) ~ many(not('\\') | escapedChar) ~ (not('\\', '"') | escapedChar) ~ word("\"" * 3) |
        word("'" * 3) ~ many(not('\\') | escapedChar) ~ (not('\\', '\'') | escapedChar) ~ word("'" * 3)

    val idStart = elem(_.isLetter) | elem('_')
    val idContinue = idStart | elem(_.isDigit)

    val digitPart = digit ~ many(opt(elem('_')) ~ digit)
    val pointFloat = opt(digitPart) ~ elem('.') ~ digitPart | digitPart ~ elem('.')
    val exponentFloat = (digitPart | pointFloat) ~ oneOf("eE") ~ opt(oneOf("+-")) ~ digitPart
    
    val lexer: Lexer = Lexer(
        elem(_.isWhitespace) |> Space(),
        elem('#') ~ many(any) ~ elem('\n') |> Comment(),

        keyword |>
            {(s, range) => Keyword(s.mkString).setPos(range._1)},
        idStart ~ many(idContinue) |>
            {(s, range) => Identifier(s.mkString).setPos(range._1)},

        opt(stringPrefix) ~ (shortString | longString) |>
            {(s, range) => makeStringLiteral(s.mkString).setPos(range._1)},
        
        // integer literals
        nonZero ~ many(opt(elem('_')) ~ digit) | many1(elem('0')) ~ many(opt(elem('_')) ~ elem('0')) |>
            {(s, range) => IntLiteral(BigInt(s.filter(_ != '_').mkString)).setPos(range._1)},
        elem('0') ~ oneOf("bB") ~ many1(opt(elem('_')) ~ oneOf("01")) |>
            {(s, range) => IntLiteral(parseBigInt(s, 2)).setPos(range._1)},
        elem('0') ~ oneOf("oO") ~ many1(opt(elem('_')) ~ elem(c => c >= '0' && c <= '7')) |>
            {(s, range) => IntLiteral(parseBigInt(s, 8)).setPos(range._1)},
        elem('0') ~ oneOf("xX") ~ many1(opt(elem('_')) ~ hex) |>
            {(s, range) => IntLiteral(parseBigInt(s, 16)).setPos(range._1)},

        // floating point and imaginary literals
        pointFloat | exponentFloat |>
            {(s, range) => FloatLiteral(s.filter(_ != '_').mkString.toFloat)},
        (pointFloat | exponentFloat | digitPart) ~ oneOf("jJ") |>
            {(s, range) => ImaginaryLiteral(s.filter(_ != '_').mkString.dropRight(1).toFloat)},

    
        operator |> {(s, range) => Operator(s.mkString).setPos(range._1)},
        delimiters |> {(s, range) => Delimiter(s.mkString).setPos(range._1)}


    )

    def run(ctx: Context)(sources: List[File]): Iterator[Token] = {
        val tokens = sources.foldLeft(Iterator[Token]())((acc, file) => {
            acc ++ lexer.spawn(
                Source.fromFile(file, SourcePositioner(file))
            )
        })
        
        tokens.filter {
            case Space() => false
            case _ => true
        }
    }
}

object PrintTokens extends Pipeline[Iterator[Token], Unit] {
    def run(ctx: Context)(tokens: Iterator[Token]) = {
        tokens.foreach(println(_))
    }
}