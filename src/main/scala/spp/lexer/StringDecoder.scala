package spp.lexer

import spp.structure.Tokens._
import sl.Lexers
import sl.Rules._
import sl.Positions._
import sl.Expressions._

import scala.language.implicitConversions
import scala.util.{Try, Success, Failure}

object StringDecoder extends Lexers {
  
  implicit def successChar(ch: Char): Positioned[Try[Char]] = positionedToken(Success(ch))

  type Token = Try[Char]
  type Value = None.type

  type LexerRule = Rule[Value, Try[Char]]

  //override val debug: Boolean = true

  val escapesRules: List[LexerRule] = List(
    unit("""\\"""+'\n') |> 
      { case(value, _) => (value, List()) },
    replace("""\\\\""", '\\'),
    replace("""\\'""", '\''),
    replace("""\\"""+'"', '\"'),
    replace("""\\a""", '\u0007'),
    replace("""\\b""", '\b'),
    replace("""\\f""", '\f'),
    replace("""\\n""", '\n'),
    replace("""\\r""", '\r'),
    replace("""\\t""", '\t'),
    replace("""\\v""", '\u000b'),
    """\\""" ~/~ "[0-7]{1,3}" |>
      { case (value, List(esc, oct)) => (value, List(Integer.parseInt(oct.str, 8).toChar)) },
    """\\x""" ~/~ "[0-9a-fA-F]{2}" |>
      { case (value, List(esc, hex)) =>
        (value, List(Integer.parseInt(hex.str, 16).toChar))
      },
    unit("""\\x.?""") |> // 
      { case (value, List(c)) => (value, List(Failure(LexerError(f"Failed to decode bytes at position ${c.start}", c.start))))}
  )

  val strOnlyEscapeRules: List[LexerRule] = List(
    """\\N\{""" ~/~ "[a-zA-Z]*" ~/~ "}" |>
      { case (value, List(open, name, close)) => (value, List(unicodeFromName(name.str))) },
    """\\u""" ~/~ "[0-9a-zA-Z]{4}" |>
      { case (value, List(esc, hex)) =>
        (value, List(Integer.parseInt(hex.str, 16).toChar))
      },
    """\\U""" ~/~ "[0-9a-zA-Z]{8}" |>
      { case (value, List(esc, hex)) =>
        (value, List(Integer.parseInt(hex.str, 16).toChar))
      },
    unit("""\\[NuU].?""") |> // 
      { case (value, List(c)) => (value, List(Failure(LexerError(f"Failed to decode bytes at position ${c.start}", c.start))))},
  )

  val defaultRule: LexerRule = unit("""\p{all}""") |> { case (value, List(c)) => (value, List(c.str.charAt(0)))}
  
  val formatRules: List[LexerRule] = List(
    replace("""\{\{""", '{'),
    replace("""}}""", '}'),
    ("""\{""" ~ ("$"|"""[^\{]\p{all}*""")) | ("}" ~ ("$"|"""[^}]\p{all}*""")) |> // if we encounter a single brace, we stop decoding
      { case (value, List(c)) => (value, List()) },
  )

  val bytesLexer = Lexer((escapesRules :+ defaultRule): _*)(None)
  val stringLexer = Lexer((escapesRules ++ strOnlyEscapeRules :+ defaultRule): _*)(None)
  val formatLexer = Lexer((escapesRules ++ strOnlyEscapeRules ++ formatRules :+ defaultRule): _*)(None)
  val rawFormatLexer = Lexer((formatRules :+ defaultRule): _*)(None)

  def unicodeFromName(name: String): Char =
      if (name == "LF") '\n'
      else if (name == "TAB") '\t'
      else '\u0000' // TODO complete

  def replace(pattern: String, replacement: Char): LexerRule =
    unit(pattern) |> { case (value, List(str)) => (value, List(replacement)) }

  def decode(prefix: String, value: String): Try[(String, Int)] = {
    val pr = prefix.toLowerCase

    if (prefix.contains("r") && !prefix.contains("f")) Success(value, value.length)
    else {
      val lexer = 
        if (prefix.contains("b")) bytesLexer
        else if (prefix.contains("f"))
          if (prefix.contains("r")) rawFormatLexer
          else formatLexer
        else stringLexer
  
      lexer.tokenizeFromString(value) match {
        case Failure(exception) => Failure(exception)
        case Success(res) => {
          // position where lexer stopped (useful for f-strings)
          val end = res.lastOption.map(_.end.index).getOrElse(0)
          val str = Try(res.map(_.value.get))
          str.map(chars => (chars.mkString, end))
        }
      }
    }
  }
}