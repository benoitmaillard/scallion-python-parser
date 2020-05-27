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
    if (prefix.contains('f')) JoinedStr(parseFormattedStr(sl.prefix, 0, sl.value)._1)
    else if (prefix.contains('b')) BytesConstant(decode(prefix, sl.value).get)
    else StringConstant(decode(prefix, sl.value).get)
  }

  // TODO: should probably return the position of the end of expr (or the remaining str ?)
  def parseFormattedStr(prefix: String, level: Int, str: String): (Seq[Expr], String) = {
    val cstPart = StringDecoder.decode(prefix, str).get
    val tailPart = str.drop(cstPart.length)

    // TODO: in the case were we encounter a '}', we must check that we are at level 0
    if (tailPart.isEmpty) (cstPartSeq(cstPart), "")
    else if (tailPart.head == '}') (cstPartSeq(cstPart), tailPart.drop(1)) // we drop the '}'
    else {
      val (fValue, remaining) = parseFromExpr(prefix, level, tailPart.drop(1)) // we drop the '{'
      (cstPartSeq(cstPart) :+ fValue, remaining)

      val (exprsAfter, remainingAfter) = parseFormattedStr(prefix, level, remaining)
      ((cstPartSeq(cstPart) :+ fValue) ++ exprsAfter, remainingAfter)
    }
  }

  def cstPartSeq(part: String): Seq[Expr] =
    if (!part.isEmpty) Seq(StringConstant(part))
    else Seq()

  // str starts with first char after '{'
  def parseFromExpr(prefix: String, level: Int, str: String): (FormattedValue, String) = {
    // TODO: should check if string is empty (missing })
    val (exprTokens, afterExpr) = extractExpr(str)

    // TODO: should check if expression is empty (and valid, think of lambda case)
    val expr = Parser.parseExpr(exprTokens)
    
    val (conversion, afterConversion) = extractConversion(afterExpr)
    val (format, afterFormat) = extractFormat(prefix, level, afterConversion)

    (FormattedValue(expr, conversion, format), afterFormat.drop(1)) // we drop terminating '}'
  }

  def extractConversion(str: String): (Option[Char], String) = str.toSeq match {
    case '!' +: c +: tail => (Some(c), tail.mkString)
    case _ => (None, str)
  }

  def extractFormat(prefix: String, level: Int, str: String): (Option[Expr], String) = str.toSeq match {
    case ':' +: format => {
      val (exprs, remaining) = parseFormattedStr(prefix, level + 1, format.mkString)
      (Some(JoinedStr(exprs)), remaining)
    }
    case _ => (None, str)
  }
  
  // Finds the tokens of the expression and the remaining input from the first char
  // after the expression (this would be either '!', ':' or '}')
  def extractExpr(str: String): (Iterator[Token], String) = {
    val tokens = Lexer.applyString(str).toList

    // we are looking for 3 things : !, :, } at level 0
    // TODO: add surrounding parentheses !!! (maybe rather in extractExpr)
    val (exprTokens, tokenAfter) = findExprEnd(tokens, List()).get

    (exprTokens.iterator, str.substring(tokenAfter.position.col))
  }

  def findExprEnd(tokens: List[Token], stack: List[String]): Try[(List[Token], Token)] = {
    def findExprEndAcc(tokens: List[Token], stack: List[String], acc: List[Token]): Try[(List[Token], Token)] = tokens match {
      case (t@Delimiter(del@("{" | "[" | "("))) :: tail =>
        findExprEndAcc(tail, del :: stack, t :: acc)
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
    findExprEndAcc(tokens, stack, List())
  }
}