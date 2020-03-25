package spp
import utils._
import spp.parsing._
import spp.lexer._
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val context = Context(new Reporter, args.toList)
    val pipeline = Lexer andThen Parser andThen TreePrinter
    
    try {
      if (context.files.isEmpty) {
        context.reporter.fatal("No source files provided")
      } else {
        pipeline.run(context){
          val file = new File(context.files.head)
          if (file.exists()) file
          else context.reporter.fatal(s"File ${file.getName()} not found")
        }
      }
      
      context.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(msg) => sys.exit(1)
    }
  }
}