package spp.parsing

import spp.utils._
import java.io.File
import scallion.lexical._
import scallion.input._

object Lexer extends Pipeline[List[File], Iterator[Token]] with Lexers with CharRegExps {
    type Token = spp.parsing.Token
    type Position = StringPosition
    
    val lexer: Lexer = ???

    def run(ctx: Context)(sources: List[File]): Iterator[Token] = {
        sources.foldLeft(Iterator[Token]())((acc, file) => {
            acc ++ lexer.spawn(Source.fromFile(file, StringPositioner))
        })
    }
}