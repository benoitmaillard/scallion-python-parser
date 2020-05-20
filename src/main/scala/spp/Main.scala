package spp
import utils._
import spp.parsing._
import spp.lexer._
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val context = Context(new Reporter, args.toList)

    try {
      
      val tokens = Lexer(context.files.head)
      val tree = Parser(context, tokens)        
      
      context.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(msg) => sys.exit(1)
    }
  }
}