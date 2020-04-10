package spp
import utils._
import spp.parsing._
import spp.lexer._
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val context = Context(new Reporter, args.toList)

    try {
      if (false) {
        context.reporter.fatal("No source files provided")
      } else {
        val file = new File("./examples/simple-input.py") // new File(context.files.head)
        if (file.exists()) {
          val tokens = Lexer(context, file)
          val tree = Parser(context, tokens)
          TreePrinter(context, tree)
        }
        else context.reporter.fatal(s"File ${file.getName()} not found")
      }
      
      context.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(msg) => sys.exit(1)
    }
  }
}