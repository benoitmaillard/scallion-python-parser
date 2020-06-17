package sl

package sl

import org.scalatest._
import scala.language.implicitConversions
import Expressions._
import Rules._

// EXAMPLE FOR REPORT

class DelimitersTest extends FlatSpec with Lexers {
  val inputExtension: String = ".txt"
  val outputExtension: String = ".txt"

  type Value = Int
  type Token = ExampleToken

  trait ExampleToken
  case class IntLiteral(value: Int) extends ExampleToken
  case object Opening extends ExampleToken
  case object Closing extends ExampleToken
  case object Space extends ExampleToken
  case class ErrorToken(msg: String) extends ExampleToken
  
  val lexer = Lexer(
    "0[xX]" ~/~ """[0-9a-fA-F]*""" |> {
      case (level, List(_, digits)) => (level, List(IntLiteral(Integer.parseInt(digits.str, 16))))
    },
    """\(""" |> {
      case (level, List(opening)) => (level + 1, List(Opening))
    },
    """\)""" |> {
      case (level, List(closing)) =>
        if (level > 0) (level - 1, List(Closing))
        else throw new Error("No matching opening parenthesis found")
    },
    """ """ |> {
      case (level, List(_)) => (level, List(Space))
    }
  )(0)

  "indentation-based lexer" should "tokenize basic file correctly" in {
    val (tokens, value) = lexer.tokenizeFromString("(0x12 (0x12))()()").get
    
    if (value > 0)
      throw new Error("Error : Unclosed parenthesis")
    
    
  }
}