package spp.parsing

import spp.structure.Tokens.StringLiteral
import spp.structure.AbstractSyntaxTree._
import spp.lexer.StringDecoder._

object StringLiteralParser {
  def parse(sl: StringLiteral): Expr = {
    val prefix = sl.prefix.toLowerCase
    if (prefix.contains('f')) parseFormattedStr(sl)
    else if (prefix.contains('b')) BytesConstant(decode(sl).get)
    else StringConstant(decode(sl).get)
  }

  def parseFormattedStr(sl: StringLiteral): JoinedStr = ???
}