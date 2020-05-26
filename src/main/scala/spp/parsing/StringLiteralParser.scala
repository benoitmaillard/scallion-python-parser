package spp.parsing

import spp.structure.Tokens.StringLiteral
import spp.structure.AbstractSyntaxTree._
import spp.lexer.StringDecoder._
import spp.lexer.Lexer

object StringLiteralParser {
  def parse(sl: StringLiteral): Expr = {
    val prefix = sl.prefix.toLowerCase
    if (prefix.contains('f')) JoinedStr(parseFormattedStr(sl.value))
    else if (prefix.contains('b')) BytesConstant(decode(prefix, sl.value).get)
    else StringConstant(decode(prefix, sl.value).get)
  }

  def parseFormattedStr(str: String): Seq[Expr] = ???

  def extractExprPart(str: String): String = ???
}