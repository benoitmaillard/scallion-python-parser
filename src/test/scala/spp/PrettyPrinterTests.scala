package spp

import org.scalatest._
import org.json4s.native.JsonParser.Parser

import spp.parsing.Parser
import spp.lexer.Lexer

class PrettyPrinterTests extends FlatSpec {
    "pretty printer" should "output a result for input with various structures" in {
        val tree = Parser(Lexer("src/test/resources/input/pprint.py"))
        assert(Parser.unapply(tree).isDefined)
    }
}