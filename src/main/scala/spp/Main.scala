package spp
import utils._
import spp.parsing._
import spp.lexer._
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    try {
      
      val tokens = Lexer(args.toList.head)
      val tree = Parser(tokens)        
    } catch {
      case AmycFatalError(msg) => sys.exit(1)
    }
  }
}