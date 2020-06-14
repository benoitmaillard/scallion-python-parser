package spp.parsing

import spp.structure.Tokens.StringLiteral
import spp.structure.AbstractSyntaxTree.{Expr, StringConstant, BytesConstant, JoinedStr, FormattedValue}
import spp.lexer.StringDecoder.decode
import spp.lexer.Lexer
import spp.lexer.StringDecoder
import spp.structure.Tokens._

import scala.util.{Try, Success, Failure}

object StringLiteralParser {
  def parse(sl: StringLiteral): Expr = {
    val prefix = sl.prefix.toLowerCase
    if (prefix.contains('f')) JoinedStr(parseFormattedStr(sl.prefix, sl.value)._1)
    else StringConstant(decode(prefix, sl.value).get._1)
  }
  // TODO: should probably return the position of the end of expr (or the remaining str ?)
  def parseFormattedStr(prefix: String, str: String): (Seq[Expr], String) = {
    val (cstPart, length) = StringDecoder.decode(prefix, str).get
    val tailPart = str.drop(length)

    // TODO: use a match instead
    if (tailPart.isEmpty) (cstPartSeq(cstPart), "")
    else if (tailPart.head == '}') (cstPartSeq(cstPart), tailPart)
    else {
      // parse the entirety of the replacement field
      val (fValue, remaining) = parseFromExpr(prefix, tailPart.drop(1)) // we drop the '{'

      // parse what remains after the replacement field
      val (exprsAfter, remainingAfter) = parseFormattedStr(prefix, remaining)

      // assemble everything together, remainingAfter is the string left to parse at a higher level
      ((cstPartSeq(cstPart) :+ fValue) ++ exprsAfter, remainingAfter)
    }
  }

  def cstPartSeq(part: String): Seq[Expr] =
    if (!part.isEmpty) Seq(StringConstant(part))
    else Seq()

  // str starts with first char after '{'
  def parseFromExpr(prefix: String, str: String): (FormattedValue, String) = {
    // TODO: should check if string is empty (missing })
    val (exprTokens, afterExpr) = extractExpr(str)

    val tokensWithParens = Delimiter("(") +: exprTokens :+ Delimiter(")")

    // TODO: should check if expression is empty (and valid, think of lambda case)
    val expr = Parser.parseExpr(tokensWithParens.iterator)
    
    val (conversion, afterConversion) = extractConversion(afterExpr)
    val (format, afterFormat) = extractFormat(prefix, afterConversion)

    (FormattedValue(expr, conversion, format), afterFormat.drop(1)) // we drop terminating '}'
  }

  def extractConversion(str: String): (Option[Char], String) = str.toSeq match {
    case '!' +: c +: tail => (Some(c), tail.mkString)
    case _ => (None, str)
  }

  def extractFormat(prefix: String, str: String): (Option[JoinedStr], String) = str.toSeq match {
    case ':' +: format => {
      val (exprs, remaining) = parseFormattedStr(prefix, format.mkString)
      (Some(JoinedStr(exprs)), remaining)
    }
    case _ => (None, str)
  }
  
  // Finds the tokens of the expression and the remaining input from the first char
  // after the expression (this would be either '!', ':' or '}')
  def extractExpr(str: String): (Seq[Token], String) = {
    val tokens = Lexer.applyString(str).toList

    // we are looking for 3 things : !, :, } at level 0
    // TODO: add surrounding parentheses !!! (maybe rather in extractExpr)
    val (exprTokens, tokenAfter) = findExprEnd(tokens, List()).get

    (exprTokens, str.substring(tokenAfter.position.col))
  }

  def findExprEnd(tokens: List[Token], stack: List[String]): Try[(List[Token], Token)] = {
    val delimiterMap = Map("{" -> "}", "[" -> "]", "(" -> ")")

    def findExprEndAcc(tokens: List[Token], stack: List[String], acc: List[Token]): Try[(List[Token], Token)] = tokens match {
      case (t@Delimiter(del@("{" | "[" | "("))) :: tail => findExprEndAcc(tail, delimiterMap(del) :: stack, t :: acc)
      case (t@Delimiter(del@("}" | "]" | ")"))) :: tail => stack match {
        case `del` :: stackTail => findExprEndAcc(tail, stackTail, t :: acc)
        case Nil => if (del == "}") Success((acc, t)) else Failure(new Error(f"Unmatched $del")) 
        case _ => Failure(new Error(f"Unmatched $del"))
      }
      case (t@Delimiter(":" | "!" )) :: tail => stack match {
        case Nil => Success((acc, t))
        case _ => findExprEndAcc(tail, stack, t :: acc)
      }
      case other :: tail => findExprEndAcc(tail, stack, other :: acc)
      case Nil => Failure(new Error(f"Unclosed format value"))
    }

    findExprEndAcc(tokens, stack, List()).map{ case (resTokens, after) => (resTokens.reverse, after) }
  }
}