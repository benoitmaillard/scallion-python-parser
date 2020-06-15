package spp
import utils._
import spp.parsing._
import spp.lexer._
import spp.structure.TreeSerializer._

import java.io.File

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import spp.structure.AbstractSyntaxTree.Module

object Main {
  def main(args: Array[String]): Unit = args(0) match {
    case "tokenize" => Lexer(args(1)).foreach(println(_))
    case "parse" => 
      println(parse(args(1)))
    case "compare" =>     
      compare(args(2), parse(args(1)))
    case "pprint" => println(Parser.unapply(parse(args(1))).get)
  }

  def parse(path: String): Module = {
    val tree = Parser(Lexer(path))
    TreeValidation.validate(tree).get
    tree
  }
}