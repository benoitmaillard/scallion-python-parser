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
  implicit def stringToDelimiter(del: String): Syntax[Token] = elem(DelimiterClass(del))
  def kw(value: String): Syntax[Token] = elem(KeywordClass(value))

  lazy val name: Syntax[Expr] = accept(NameClass) {
    case Identifier(name) => Name(None, name)
  }
  

  lazy val module = (many(stmt) ~ EOFClass) map { // TODO doc says there could be NEWLINE only ??
    case seq ~ eof => Module(seq.reduce(_ ++ _))
  }

  lazy val stmt: Syntax[Seq[Statement]] = simpleStmt /*| compoundStmt.map(st => Seq(st))*/

  // One or more smallStmt on a single line
  lazy val simpleStmt: Syntax[Seq[Statement]] =
    (smallStmt ~ many(";" ~ smallStmt) /*~ opt(";")*/ ~ NewlineClass) map {
      case firstStmt ~ seqStmts ~ _ /*~ _*/ => firstStmt +: seqStmts.map{case _ ~ stmt => stmt}
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

  // TODO complete
  lazy val exprStmt: Syntax[Statement] = (testListStarExpr ~ many1("=" ~ testListStarExpr)) map {
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

  lazy val namedExprTest: Syntax[Expr] = ???
  lazy val test: Syntax[Expr] = recursive(
    (orTest ~ opt(kw("if") ~ orTest ~ kw("else") ~ test)).map {
      case vTrue ~ Some(_ ~ condition ~ _ ~ vFalse) => TernaryExpr(condition, vTrue, vFalse)
      case v ~ None => v
    }
  )

  lazy val orTest: Syntax[Expr] = name

  lazy val testListStarExpr: Syntax[Seq[Expr]] =
    ((test /* | starExpr*/) ~ many("," ~ (test /* | starExpr*/)) /*~ opt(",")*/) map {
      case exp1 ~ seq /*~ _*/ => exp1 +: seq.map{case _ ~ e => e}
    }

  /*
  lazy val expr: Syntax[Expr] = operators(factor)(
    binOp("*") | binOp("@") is LeftAssociative
  )( {
    case (l, Operator(op), r) => BinaryExpr(op, l, r)
  }

  )*/

  // lazy val factor: Syntax[Expr] = ???

  
  def run(ctx: Context)(v: Iterator[Token]): Module = {
    if (!module.isLL1) {
      debug(module)
      ctx.reporter.fatal("Not LL1!!")
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