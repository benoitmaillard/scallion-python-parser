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
    val pipeline = path => TokensToString(Lexer(path))
    
    "lexer" should "tokenize basic input file correctly" in {
        outputMatch("basic-test")
    }

    it should "tokenize file with blank lines correctly" in {
        outputMatch("empty-lines-test")
    }

    it should "produce only EOF token for empty file" in {
        outputMatch("empty")
    }

    it should "tokenize a single-line input file correctly" in {
        outputMatch("single-line")
    }
    
}

object TokensToString {
    def apply(tokens: Iterator[Token]) = {
        tokens.map(_.toString()).reduce(_ ++ _)
    }
}