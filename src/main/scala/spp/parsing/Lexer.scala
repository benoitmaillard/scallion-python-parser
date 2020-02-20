package spp.parsing

import spp.utils._
import spp.structure._

import java.io.File
import scallion.lexical._
import scallion.input._

object Lexer extends Pipeline[List[File], Iterator[Tokens.Token]] with Lexers with CharRegExps {
    type Token = Tokens.Token
    type Position = StringPosition
    
    val lexer: Lexer = ???

    def run(ctx: Context)(sources: List[File]): Iterator[Token] = {
        sources.foldLeft(Iterator[Token]())((acc, file) => {
            acc ++ lexer.spawn(Source.fromFile(file, StringPositioner))
        })
    }
}