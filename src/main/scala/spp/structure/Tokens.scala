package spp.structure

import spp.utils.Positioned
import math.BigInt

object Tokens {
  sealed abstract class Token extends Positioned with Product {
    override def toString(): String = {
      productPrefix + productIterator.mkString("(", ",", ")")
    }
  }
  
  // TODO intermediary tokens for indent/dedent
  final case class Newline() extends Token
  final case class Indent() extends Token
  final case class Dedent() extends Token
  
  final case class Keyword(name: String) extends Token
  final case class Identifier(name: String) extends Token
  
  final case class StringLiteral(prefix: Option[String], value: String) extends Token
  // final case class FormatStringLiteral(value: String) extends Token
  final case class BytesLiteral(value: String) extends Token
  
  final case class IntLiteral(value: BigInt) extends Token
  final case class FloatLiteral(value: Float) extends Token
  final case class ImaginaryLiteral(value: Float) extends Token
  
  final case class Operator(op: String) extends Token
  final case class Delimiter(del: String) extends Token
  
  final case class EOF() extends Token
  
  // Intermediary tokens
  final case class Space() extends Token
  final case class Comment() extends Token
  final case class ErrorToken(msg: String) extends Token
  final case class PhysicalIndent(length: Int) extends Token
}

object TokenClasses {
  sealed abstract class TokenClass(val display: String) extends Positioned {

  }  
  final case object NewlineClass extends TokenClass("\n")
  final case object IndentClass extends TokenClass("<indent>")
  final case object DedentClass extends TokenClass("<dedent>")
  final case class KeywordClass(value: String) extends TokenClass(value)
  final case object NameClass extends TokenClass("<name>")
  final case object NumberClass extends TokenClass("<number>")
  final case object StringClass extends TokenClass("<string>")

  final case class OperatorClass(value: String) extends TokenClass(value)
  final case class DelimiterClass(value: String) extends TokenClass(value)
  final case object EOFClass extends TokenClass("<EOF>")

  final case object OtherClass extends TokenClass("<other>")
}