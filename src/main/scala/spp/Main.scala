package spp
import utils._
import spp.parsing._
import spp.lexer._
import spp.structure.TreeSerializer._

import java.io.File

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

object Main {
  def main(args: Array[String]): Unit = args(0) match {
    case "tokenize" => Lexer(args(1)).foreach(println(_))
    case "parse" => {

      val toMilli = Math.pow(10.0, -6.0)
      
      val startT = System.nanoTime
      val tokens = Lexer(args(1))

      val lexerT = (System.nanoTime - startT) * toMilli

      val tree = Parser(tokens)

      val totalT = (System.nanoTime - startT) * toMilli
      println("Execution time")
      println(f"  - Lexer : $lexerT")
      println(f"  - Total : $totalT")
    }
    case "compare" => {
      val tokens = Lexer(args(1))
      val tree = Parser(tokens)
      
      compare(args(2), tree)
    }
  }
}