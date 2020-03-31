package spp.parsing

import scallion.syntactic._

import spp.utils._
import spp.structure._
import spp.structure.AbstractSyntaxTree._
import spp.parsing._
import spp.structure._
import spp.structure.Tokens._
import spp.structure.TokenClasses._

import scala.language.implicitConversions

object Parser extends Pipeline[Iterator[Tokens.Token], Module]
with Syntaxes with ll1.Parsing with Operators with ll1.Debug  {
  type Token = Tokens.Token
  type Kind = TokenClass

  implicit def classToSyntax(k: Kind): Syntax[Token] = elem(k)

  def kw(value: String): Syntax[String] = accept(KeywordClass(value)) {
    case _ => value
  }

  def del(del: String): Syntax[Token] = elem(DelimiterClass(del))

  def compKw(value1: String, value2: String): Syntax[String] = 
    kw(value1) ~ kw(value2) map {
      case v1 ~ v2 => v1 ++ " " ++ v2
    }

  def binaryOp(value: String): Syntax[String] = accept(OperatorClass(value)) {
    case _ => value
  }

  def op(value: String): Syntax[Token] = elem(OperatorClass(value))

  def oneOfOp(values: String*) = values.map(binaryOp(_)).reduce(_ | _)

  lazy val name: Syntax[Expr] = accept(NameClass) {
    case Identifier(name) => Name(None, name)
  }
  

  lazy val module = (many(stmt) ~ EOFClass) map { // TODO doc says there could be NEWLINE only ??
    case seq ~ eof => Module(seq.reduce(_ ++ _))
  }

  lazy val stmt: Syntax[Seq[Statement]] = simpleStmt /*| compoundStmt.map(st => Seq(st))*/

  // One or more smallStmt on a single line
  lazy val simpleStmt: Syntax[Seq[Statement]] = 
    (simpleStmtWithoutNewLine ~ NewlineClass) map {
      case s ~ _ => s
    }
  lazy val simpleStmtWithoutNewLine: Syntax[Seq[Statement]] = recursive(
    (smallStmt ~ opt(simpleStmtFollow)) map {
      case s1 ~ ssOpt => s1 +: ssOpt.getOrElse(Seq.empty)
    })
  lazy val simpleStmtFollow: Syntax[Seq[Statement]] =
    (del(";") ~ opt(simpleStmtWithoutNewLine)) map {
      case _ ~ sOpt => sOpt.getOrElse(Seq.empty)
    }

  /*lazy val compoundStmt: Syntax[Statement] = ifStmt | whileStmt | forStmt | tryStmt
    withStmt | funcDef | classDef | decorated | asyncStmt

  lazy val ifStmt: Syntax[Statement] = ???
  lazy val whileStmt: Syntax[Statement] = ???
  lazy val forStmt: Syntax[Statement] = ???
  lazy val tryStmt: Syntax[Statement] = ???
  lazy val withStmt: Syntax[Statement] = ???
  lazy val funcDef: Syntax[Statement] = ???
  lazy val classDef: Syntax[Statement] = ???
  lazy val decorated: Syntax[Statement] = ???
  lazy val asyncStmt: Syntax[Statement] = ???*/

  lazy val smallStmt: Syntax[Statement] = exprStmt /*| delStmt | passStmt | flowStmt*/

  lazy val exprStmt: Syntax[Statement] = (testListStarExpr ~ many1(del("=") ~ testListStarExpr)) map {
    case seqExpr ~ seqSeqExpr => {
      val removedEquals = seqSeqExpr.map{ case _ ~ testList => testList}
      Assign(seqExpr +: removedEquals.init, removedEquals.last)
    }
  }
  /*
  lazy val delStmt: Syntax[Statement] = ???
  lazy val passStmt: Syntax[Statement] = ???
  lazy val flowStmt: Syntax[Statement] = ???
  lazy val importStmt: Syntax[Statement] = ???
  lazy val globalStmt: Syntax[Statement] = ???
  lazy val nonLocalStmt: Syntax[Statement] = ???
  lazy val assertStmt: Syntax[Statement] = ???*/
  
  // What follows ':' (example after an if ...: or for ...:)
  lazy val suiteStmt: Syntax[Seq[Statement]] =
    simpleStmt | (NewlineClass ~ IndentClass ~ many1(stmt) ~ DedentClass).map {
      case _ ~ _ ~ seqSeqStmts ~ _ => seqSeqStmts.reduce(_ ++ _)
    }

  // lazy val namedExprTest: Syntax[Expr] = ???
  lazy val test: Syntax[Expr] = recursive(
    orTest ~ opt(kw("if") ~ orTest ~ kw("else") ~ test) /* lambdef*/ map {
      case vTrue ~ Some(_ ~ condition ~ _ ~ vFalse) => IfExpr(condition, vTrue, vFalse)
      case v ~ None => v
    }
  )

  lazy val orTest: Syntax[Expr] = operators(notTest)(
    kw("or") is LeftAssociative,
    kw("and") is LeftAssociative
  ) {
    case (l, op, r) => BoolOp(op, l, r)
  }

  lazy val notTest: Syntax[Expr] = recursive(
    kw("not") ~ notTest map {
      case _ ~ e => Not(e)
    }
  ).up[Expr] | comparison

  // comparisons can be chained : 1 < 2 < x == 3
  lazy val comparison: Syntax[Expr] = expr ~ many(compOp ~ expr) map {
    case e1 ~ Nil => e1
    case e1 ~ l => Compare(e1, l.map { case op ~ e => op }, l.map { case op ~ e => e } )
  }

  lazy val compOp =
    oneOfOp("<", ">", "==", ">=", "<=", "<>", "!=") |
    kw("in") | compKw("not", "in") | isOperator

  // Special case for LL1
  lazy val isOperator = kw("is") ~ opt(kw("not")) map {
    case _ ~ Some(_) => "is not"
    case _ ~ None => "is"
  }

  lazy val starExpr = op("*") ~ expr map { case _ ~ e => e }

  lazy val expr: Syntax[Expr] = operators(atomExpr)(
    binaryOp("|") is LeftAssociative,
    binaryOp("^") is LeftAssociative,
    binaryOp("&") is LeftAssociative,
    oneOfOp("<<", ">>") is LeftAssociative,
    oneOfOp("*", "@", "/", "%", "//") is LeftAssociative,
    oneOfOp("+", "-", "~") is LeftAssociative,
    binaryOp("**") is RightAssociative
  ) {
    case (l, op, r) => BinOp(op, l, r)
  }

  lazy val atomExpr: Syntax[Expr] = atom
  lazy val atom: Syntax[Expr] = name
  // lazy val trailer: Syntax[Expr] = ???
  
  lazy val testListStarExpr: Syntax[Seq[Expr]] = recursive(
    (test /* | starExpr*/) ~ opt(testListStarExprFollow) map {
      case t ~ tListOpt => t +: tListOpt.getOrElse(Seq.empty)
    })

  lazy val testListStarExprFollow: Syntax[Seq[Expr]] =
    del(",") ~ opt(testListStarExpr) map {
      case _ ~ tListOpt => tListOpt.getOrElse(Seq.empty)
    }
  
  def run(ctx: Context)(v: Iterator[Token]): Module = {
    if (!module.isLL1) {
      debug(module)
      ctx.reporter.fatal("Syntax is not LL1!")
    } else {
      println("Syntax is LL1!")
    }
    val parser = LL1(module)
    
    parser(v) match {
      case LL1.Parsed(value, rest) => value
      case LL1.UnexpectedToken(token, rest) => {
        println(token)
        println(rest)
        ctx.reporter.fatal("test")
      }
      case LL1.UnexpectedEnd(rest) => ???
    }
  }
  
  def getKind(token: Token): Kind = token match {
    case _:StringLiteral | _:BytesLiteral | _:IntLiteral |
      _:FloatLiteral | _:ImaginaryLiteral => LiteralClass
    case Keyword(name) => KeywordClass(name)
    case Operator(op) => OperatorClass(op)
    case Identifier(name) => NameClass
    case Delimiter(del) => DelimiterClass(del)
    case Newline() => NewlineClass
    case EOF() => EOFClass
    case _ => OtherClass
  }
}