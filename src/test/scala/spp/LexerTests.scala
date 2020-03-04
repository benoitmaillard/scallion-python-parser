package spp

import org.scalatest._
import java.io.File
import spp.utils.Pipeline
import spp.lexer.Lexer
import spp.structure.Tokens.Token
import spp.structure.Tokens.Token
import spp.utils.Context

class LexerTests extends OutputComparisonSpec {
    
    val inputExtension: String = ".py"
    val outputExtension: String = ".txt"
    val pipeline = Lexer andThen TokensToString
    
    "lexer" should "tokenize basic input file correctly" in {
        outputMatch("basic-test")
    }

    it should "tokenize file with blank lines correctly" in {
        outputMatch("empty-lines-test")
    }
    
}

object TokensToString extends Pipeline[Iterator[Token], String] {
    def run(ctx: Context)(tokens: Iterator[Token]) = {
        tokens.map(_.toString()).reduce(_ ++ _)
    }
}