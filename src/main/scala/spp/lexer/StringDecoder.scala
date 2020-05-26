package spp.lexer

import spp.structure.Tokens._
import sl.Lexers
import sl.Expressions._

import scala.language.implicitConversions
import scala.util.{Try, Success, Failure}

object StringDecoder extends Lexers {
  
  implicit def successChar(ch: Char): Try[Char] = Success(ch)

  type Token = Try[Char]
  type Value = None.type
  //override val debug: Boolean = true

  val strRules = RuleSet(
    unit("""\\"""+'\n') |> 
      { case(value, _, _) => (value, List()) },
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
      { case (value, esc ~ oct, pos) => (value, List(Positioned(Integer.parseInt(oct, 8).toChar, pos))) },
    """\\x""" ~/~ "[0-9a-fA-F]{2}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    """\\N\{""" ~/~ "[a-zA-Z]*" ~/~ "}" |>
      { case (value, open ~ name ~ close, pos) => (value, List(Positioned(unicodeFromName(name), pos))) },
    """\\u""" ~/~ "[0-9a-zA-Z]{4}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    """\\U""" ~/~ "[0-9a-zA-Z]{8}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    unit("""\\[xNuU].?""") |> // 
      { (value, c, pos) => (value, List(Positioned(Failure(new Error(f"Failed to decode bytes at position ${pos}")), pos)))},
    unit("""\p{all}""") |> { (value, c, pos) => (value, List(Positioned(c.charAt(0), pos)))},
  )

  val formatStrRules = RuleSet(
    unit("""\\"""+'\n') |> 
      { case(value, _, _) => (value, List()) },
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
    replace("""\{\{""", '{'),
    replace("""}}""", '}'),
    """\\""" ~/~ "[0-7]{1,3}" |>
      { case (value, esc ~ oct, pos) => (value, List(Positioned(Integer.parseInt(oct, 8).toChar, pos))) },
    """\\x""" ~/~ "[0-9a-fA-F]{2}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    """\\N\{""" ~/~ "[a-zA-Z]*" ~/~ "}" |>
      { case (value, open ~ name ~ close, pos) => (value, List(Positioned(unicodeFromName(name), pos))) },
    """\\u""" ~/~ "[0-9a-zA-Z]{4}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    """\\U""" ~/~ "[0-9a-zA-Z]{8}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    unit("""\\[xNuU].?""") |> // 
      { (value, c, pos) => (value, List(Positioned(Failure(new Error(f"Failed to decode bytes at position ${pos}")), pos)))},
    
    unit("""\{[^\{]\p{all}*$""") |> // if we encounter a single brace, we stop decoding
      { (value, c, pos) => (value, List()) },
    unit("""\p{all}""") |> { (value, c, pos) => (value, List(Positioned(c.charAt(0), pos)))},
  )

  val bytesRules = RuleSet(
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
      { case (value, esc ~ oct, pos) => (value, List(Positioned(Integer.parseInt(oct, 8).toChar, pos))) },
    """\\x""" ~/~ "[0-9a-fA-F]{2}" |>
      { case (value, esc ~ hex, pos) =>
        (value, List(Positioned(Integer.parseInt(hex, 16).toChar, pos)))
      },
    unit("""\\x.?""") |> // 
      { (value, c, pos) => (value, List(Positioned(Failure(new Error(f"Failed to decode bytes at position ${pos}")), pos)))},
    unit("""\p{all}""") |> { (value, c, pos) => (value, List(Positioned(c.charAt(0), pos)))},
  )

  val error = Failure(new Error(f"Unable to decode"))

  val bytesLexer = Lexer(LexerState(bytesRules, None), error)
  val stringLexer = Lexer(LexerState(strRules, None), error)
  val formatLexer = Lexer(LexerState(formatStrRules, None), error)

  def unicodeFromName(name: String): Char =
      if (name == "LF") '\n'
      else if (name == "TAB") '\t'
      else '\u0000' // TODO complete

  def replace(pattern: String, replacement: Char): Rule[String] =
    unit(pattern) |> { (value, str, pos) => 
      { (value, List(Positioned(replacement, pos)))} }

  def decode(prefix: String, value: String): Try[String] = {
    val pr = prefix.toLowerCase

    if (prefix.contains("r")) Success(value)
    else {
      val lexer = 
        if (prefix.contains("b")) bytesLexer
        else if (prefix.contains("f")) {
          println("formatin string !!")
          formatLexer
        } 
        else stringLexer
  
      val tokens = lexer.tokenizeFromString(value).get.map(_.value)

      Try(tokens.map(_.get).mkString)
    }
  }
}