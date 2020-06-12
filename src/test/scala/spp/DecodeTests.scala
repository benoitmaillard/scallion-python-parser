package spp

import org.scalatest._
import java.io.File
import spp.utils.Pipeline
import spp.lexer.Lexer
import spp.structure.Tokens.Token
import spp.structure.Tokens.Token
import spp.utils.Context
import spp.structure.Tokens.StringLiteral
import spp.structure.Tokens.BytesLiteral
import spp.lexer.StringDecoder

class DecodeTests extends OutputComparisonSpec {
    
    val inputExtension: String = ".py"
    val outputExtension: String = ".txt"
    val pipeline = path => DecodeToString(Lexer(path))
    
    "decoder" should "handle various strings correctly" in {
        outputMatch("string-decode")
    }

    
}

object DecodeToString {
    def apply(tokens: Iterator[Token]) = {
        tokens.filter({
            case _:StringLiteral => true
            case _ => false
        }).map({
            case s:StringLiteral =>  StringDecoder.decode(s.prefix, s.value).get._1.map(c => c.toInt).mkString(";") + ";"
        }).mkString("\n")
    }
}