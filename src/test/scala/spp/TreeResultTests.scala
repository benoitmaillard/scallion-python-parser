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
                  BinOp("*", IntConstant(_),Call(Name("int"),Seq(PosArg(StringConstant("test")))))
                ),
                BinOp("%",IntConstant(_),IntConstant(_))),
              Seq(">"),
              Seq(IntConstant(_))
            ),
            Compare(Name("x"), Seq("=="), Seq(Name("True")))
          )
        ),
        ExprStmt(BinOp("-", Name("x"), UnaryOp("-", IntConstant(_)))),
        ExprStmt(BinOp("*", Name("x"), BinOp("**", Name("x"), BinOp("**", IntConstant(_), IntConstant(_)))))
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

  it should "produce correct tree for lambda definition" in {
    tree("lambda") should matchPattern {
      case Module(Seq(ExprStmt(
        Lambda(
          Arguments(
            Seq(
              Arg("x", None, None),
              Arg("y", None, Some(IntConstant(_)))
            ), None, Seq(), None
          ),
          BinOp("+", Name("x"), Name("y"))
        )
      ))) =>
    }
  }

  it should "produce correct tree for set and dictionary literals" in {
    tree("dict-sets") should matchPattern {
      case Module(Seq(
        Assign(Seq(Name("x")), Set(Seq(IntConstant(_), IntConstant(_), IntConstant(_)))),
        Assign(Seq(Name("x")), SetComp(Name(i), Seq(Comprehension(Name("i"), Call(Name(range), Seq(PosArg(IntConstant(_)), PosArg(IntConstant(_)))), Seq())))),
        Assign(Seq(Name("x")), Dict(Seq(KeyVal(Some(IntConstant(_)), IntConstant(_)), KeyVal(Some(IntConstant(_)), IntConstant(_))))),
        Assign(Seq(Name("x")), Dict(Seq(KeyVal(None, Name("dic"))))),
        Assign(Seq(Name("x")), Dict(Seq(KeyVal(None, Name("dic")), KeyVal(Some(IntConstant(_)), IntConstant(_))))),
        Assign(Seq(Name("x")), Set(Seq(Starred(Name(l)), IntConstant(_), IntConstant(_)))),
        Assign(Seq(Name("x")), DictComp(
          KeyVal(Some(Name("key")), Name("value")),
          Seq(Comprehension(
            Tuple(Seq(Name("key"), Name("value"))),
            Call(Name("zip"), Seq(PosArg(Name("keys")), PosArg(Name("values")))),
            Seq())))
          )
        )) =>
    }
  }

  it should "produce correct tree for decorated classes and functions" in {
    tree("decorated") should matchPattern {
      case Module(Seq(
        ClassDef("Test", Seq(PosArg(Name("object"))), Seq(
          FunctionDef("method",
            Arguments(Seq(), None, Seq(), None),
            Seq(Return(Some(IntConstant(_)))),
            Seq(Name("staticmethod")),None
          )
        ), Seq(Call(Attribute(Attribute(Name("some"), "random"), "decorator"), Seq(PosArg(IntConstant(_)), PosArg(IntConstant(_)))))
        )
      )) =>
    }
  }

  it should "produce correct tree for call arguments" in {
    tree("call-args") should matchPattern {
      case Module(Seq(
        ExprStmt(Call(Name("fun"), Seq(
          PosArg(GeneratorExp(Name("x"),Seq(Comprehension(Name("x"), List(Seq(IntConstant(_), IntConstant(_), IntConstant(_))), Seq()))))
        ))),
        ExprStmt(Call(Name("fun"),Seq(
          PosArg(Name("a")),
          PosArg(Starred(Name("b"))),
          KeywordArg(None,Name("c")),
          KeywordArg(Some(Name("test")), IntConstant(_))
        )))
      )) =>
    }
  }

  def tree(path: String): Module = {
      val base = "src/test/resources/input/tree/"

      Parser(Lexer(base + path + ".py"))
  }
}