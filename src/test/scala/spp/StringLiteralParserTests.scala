package spp
import org.scalatest._
import spp.structure.Tokens.StringLiteral
import spp.parsing.StringLiteralParser
import spp.structure.AbstractSyntaxTree._

class StringLiteralParserTests extends FlatSpec {    
  "string parser" should "handle the empty string" in {
    val trivial = StringLiteral("f", "'", """""")
    assert(StringLiteralParser.parse(trivial) match {
      case JoinedStr(Seq()) => true
      case _ => false
    })
  }

  it should "handle the trivial case" in {
    val trivial = StringLiteral("f", "'", """this is a test""")
    assert(StringLiteralParser.parse(trivial) match {
      case JoinedStr(Seq(StringConstant("this is a test"))) => true
      case _ => false
    })
  }

  it should "handle basic f-strings correctly" in {
    val value = StringLiteral("f", "'", """this is a test {1}""")
    assert(StringLiteralParser.parse(value) match {
      case JoinedStr(Seq(
        StringConstant("this is a test "),
        FormattedValue(IntConstant(v), None, None) // BigInt has no unapply
      )) => v == BigInt(1)
      case _ => false
    })
  }

  it should "handle f-strings with conversion/format correclty" in {
    val value = StringLiteral("f", "'", """this is a test {1!s:2}""")
    assert(StringLiteralParser.parse(value) match {
      case JoinedStr(Seq(
        StringConstant("this is a test "),
        FormattedValue(IntConstant(v), Some('s'), Some(StringConstant("2"))) // BigInt has no unapply
      )) => v == BigInt(1)
      case _ => false
    })
  }

  it should "handle f-strings with multiple constant and format parts" in {
    val value = StringLiteral("f", "'", """this is a test {1} ... {2} end""")
    assert(StringLiteralParser.parse(value) match {
      case JoinedStr(Seq(
        StringConstant("this is a test "),
        FormattedValue(IntConstant(v1), None, None),
        StringConstant(" ... "),
        FormattedValue(IntConstant(v2), None, None),
        StringConstant(" end")
      )) => v1 == BigInt(1) && v2 == BigInt(2)
      case _ => false
    })
  }
}
