package spp
import utils._
import spp.parsing._
import java.io.File

object Main {
    def main(args: Array[String]): Unit = {
        val context = Context(new Reporter, args.toList)
        val pipeline = Lexer andThen PrintTokens

        try {
            if (context.files.isEmpty) {
                context.reporter.fatal("No source files provided")
            } else {
                pipeline.run(context)(
                    context.files.map(name => {
                        val file = new File(name)
                        if (file.exists()) file
                        else context.reporter.fatal(s"File $name not found") 
                    })
                )
            }
            
            context.reporter.terminateIfErrors()
        } catch {
            case AmycFatalError(msg) => sys.exit(1)
        }
    }
}