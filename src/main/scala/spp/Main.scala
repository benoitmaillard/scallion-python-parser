package spp
import utils._
import spp.parsing._
import spp.lexer._
import spp.structure.TreeSerializer._
import scala.util.{Try, Success, Failure}

import java.io.File

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import spp.structure.AbstractSyntaxTree.Module

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

    case "parsedir" => {
      parseDir(args(1))
    }
  }

  def parse(file: String): Option[(Double, Double)] = Try {
    val start = System.nanoTime
    val tokens = Lexer(file)

    val lexerT = (System.nanoTime - start) * 1.0E-6
    val tree = Parser(tokens)
    val totalT = (System.nanoTime - start) * 1.0E-6
    (lexerT, totalT)
  } match {
    case Success((lexerT, totalT)) => Some(lexerT, totalT)
    case Failure(e) => println(e.getMessage()); None
  }

  def parseDir(dirPath: String) = {
    val dir = new File(dirPath)
    val files = recursiveListFiles(dir).map(_.getAbsolutePath())
    val pythonFiles = files.filter(_.matches(""".*\.py"""))
    println(f"Starting parsing of ${pythonFiles.size} files")

    //pythonFiles.foreach(println(_))
    val results = pythonFiles.map(parse(_))

    val successes = results.collect{
      case Some((lexer, total)) => (lexer, total)
    }

    val failures = results.collect{
      case None =>
    }.size

    val lexerT = successes.map(_._1).sum
    val totalT = successes.map(_._2).sum

    println("Results")
    println(f"- Failures : $failures")
    println(f"- Success ${successes.size}")
    println("Measured execution time")
    println(f"- Lexer : $lexerT")
    println(f"- Total : $totalT")
  }
  
  // code taken from https://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
}