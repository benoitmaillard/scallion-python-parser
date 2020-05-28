package spp

import org.scalatest._
import spp.lexer.Lexer
import spp.parsing.Parser
import spp.structure.AbstractSyntaxTree._

class TreeResultTests extends FlatSpec with Matchers {
  "parser" should "produce correct tree for basic expressions" in {
    tree("basic-expressions") should matchPattern {
      case Module(Seq(
        ExprStmt(
          BoolOp("and",
            Compare(
              BinOp("+",
                BinOp("+",
                  Name("x"),
                  BinOp("*", IntConstant(_),Call(Name("int"),Vector(PosArg(StringConstant("test")))))
                ),
                BinOp("%",IntConstant(_),IntConstant(_))),
              Seq(">"),
              Seq(IntConstant(_))
            ),
            Compare(Name("x"), Seq("=="), Seq(Name("True")))
          )
        )
      )) =>
    }
  }

  it should "produce correct tree for function definitions" in {
    tree("function-definitions") should matchPattern {
      case Module(Seq(
        FunctionDef("function",
          Arguments(
            Seq(
              Arg("arg1",None,None),
              Arg("arg2",None,Some(IntConstant(_)))
            ),
            Some(Arg("varargs",None,None)),
            Seq(
              Arg("kwonly1",None,None),
              Arg("kwonly2",None,None)
            ),
            Some(Arg("kwargs",None,None))),
            Seq(
              Return(Some(IntConstant(_))
            )
          ),
          Seq(),
          None
        )
      )) =>
    }
  }

  def tree(path: String): Module = {
      val base = "src/test/resources/input/tree/"

      Parser(Lexer(base + path + ".py"))
  }
}



