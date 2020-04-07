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
with Syntaxes with ll1.Parsing with Operators with ll1.Debug with PrettyPrinting  {
  type Token = Tokens.Token
  type Kind = TokenClass

  // TODO necessary ?
  trait AtomTrailer
  case class CallArgsTrailer(args: Seq[CallArg]) extends AtomTrailer
  case class SubscriptTrailer(slices: Slice) extends AtomTrailer
  case class NameTrailer(name: String) extends AtomTrailer

  trait TestListCompTrailer
  case class CompForTrailer(comp: Seq[Comprehension]) extends TestListCompTrailer
  case class ExprSeqTrailer(seq: Seq[Expr]) extends TestListCompTrailer
  implicit def classToSyntax(k: Kind): Syntax[Token] = elem(k)

  def kw(value: String): Syntax[String] = accept(KeywordClass(value)){ case _ => value }

  def del(value: String): Syntax[String] = accept(DelimiterClass(value)){ case _ => value }

  def compKw(value1: String, value2: String): Syntax[String] = 
    kw(value1) ~ kw(value2) map {
      case v1 ~ v2 => v1 ++ " " ++ v2
    }

  def op(value: String): Syntax[String] = accept(OperatorClass(value)){ case _ => value }

  def oneOfOp(values: String*) = values.map(op(_)).reduce(_ | _)

  def singleExprOrTuple(exps: Seq[Expr]) = exps match {
    case Nil => Tuple(Seq.empty)
    case head :: Nil => head
    case seq => Tuple(seq)
  }

  lazy val nameString: Syntax[String] = accept(NameClass){ case Identifier(name) => name }

  lazy val name: Syntax[Expr] = nameString map (n => Name(n))

  lazy val number: Syntax[Expr] = accept(NumberClass) {
    case IntLiteral(v) => IntConstant(v)
    case FloatLiteral(v) => FloatConstant(v)
    case ImaginaryLiteral(v) => ImaginaryConstant(v)
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
  lazy val yieldStmt: Syntax[ExprStmt] = yieldExpr map (yieldExp => ExprStmt(yieldExp))
  
  // What follows ':' (example after an if ...: or for ...:)
  lazy val suiteStmt: Syntax[Seq[Statement]] =
    simpleStmt | (NewlineClass ~ IndentClass ~ many1(stmt) ~ DedentClass).map {
      case _ ~ _ ~ seqSeqStmts ~ _ => seqSeqStmts.reduce(_ ++ _)
    }

  lazy val namedExprTest: Syntax[Expr] = test ~ opt(op(":=") ~ test) map {
    case t ~ None => t
    case t1 ~ Some(_ ~ t2) => NamedExpr(t1, t2)
  }

  // lazy val namedExprTest: Syntax[Expr] = ???
  lazy val test: Syntax[Expr] = recursive(
    orTest ~ opt(kw("if") ~ orTest ~ kw("else") ~ test) /* lambdef*/ map {
      case vTrue ~ Some(_ ~ condition ~ _ ~ vFalse) => IfExpr(condition, vTrue, vFalse)
      case v ~ None => v
    }
  )

  lazy val testNoCond: Syntax[Expr] = orTest /*lambdef_no_cond*/

  lazy val orTest: Syntax[Expr] = operators(notTest)(
    kw("or") is LeftAssociative,
    kw("and") is LeftAssociative
  ) {
    case (l, op, r) => BoolOp(op, l, r)
  }

  lazy val notTest: Syntax[Expr] = recursive(
    kw("not") ~ notTest map {
      case _ ~ e => UnaryOp("not", e)
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

  lazy val starExpr: Syntax[Expr] = op("*") ~ expr map { case _ ~ e => Starred(e) }

  lazy val expr: Syntax[Expr] = operators(atomExpr)(
    op("|") is LeftAssociative,
    op("^") is LeftAssociative,
    op("&") is LeftAssociative,
    oneOfOp("<<", ">>") is LeftAssociative,
    oneOfOp("*", "@", "/", "%", "//") is LeftAssociative,
    oneOfOp("+", "-", "~") is LeftAssociative,
    op("**") is RightAssociative
  ) {
    case (l, op, r) => BinOp(op, l, r)
  }

  lazy val atomExpr: Syntax[Expr] =
    /* await */ atom ~ (
      many(trailer)
    ) map {
    case e ~ Nil => e
    case e ~ trailers => trailers.foldLeft(e)((previousExp, trailer) => trailer match {
      case CallArgsTrailer(args) => Call(previousExp, args) // previousExp(args)
      case SubscriptTrailer(slice) => Subscript(previousExp, slice)
      case NameTrailer(name) => Attribute(previousExp, name) 
    });
  }

  // TODO comprehensions, ellipsis and strings
  lazy val atom: Syntax[Expr] =
    recursive (name | number | atomPredef | atomParens | atomBrackets)

  lazy val atomParens: Syntax[Expr] = del("(") ~ opt(yieldExpr | atomParensContent) ~ del(")") map {
    case _ ~ None ~ _ => Tuple(Seq.empty) // empty parenthesis is the 0-tuple
    case _ ~ Some(e) ~ _ => e
  }
  
  lazy val atomParensContent: Syntax[Expr] = testListComp map {
    case (e, CompForTrailer(compSeq)) => GeneratorExp(e, compSeq)
    case (e, ExprSeqTrailer(exprSeq)) => exprSeq match {
      case Nil => e
      case seq => Tuple(e +: seq)
    }
  }

  lazy val atomBrackets: Syntax[Expr] =
    del("[") ~ opt(atomBracketsContent) ~ del("]") map {
      case _ ~ None ~ _ => List(Seq.empty)
      case _ ~ Some(e) ~ _ => e
    }

  lazy val atomBracketsContent: Syntax[Expr] = testListComp map {
    case (e, CompForTrailer(compSeq)) => GeneratorExp(e, compSeq)
    case (e, ExprSeqTrailer(exprSeq)) => exprSeq match {
      case Nil => e
      case seq => List(e +: seq)
    }
  }


  
  lazy val atomPredef: Syntax[Expr] = (kw("None") | kw("True") | kw("False")) map {
    case v => Name(v)
  }
  
  lazy val trailer: Syntax[AtomTrailer] = trailerCall | trailerSubscript | trailerName
  lazy val trailerCall: Syntax[AtomTrailer] =
    del("(") ~ opt(argList) ~ del(")") map { case _ ~ o ~ _ => CallArgsTrailer(o.getOrElse(Nil))}

  lazy val trailerSubscript: Syntax[AtomTrailer] =
    del("[") ~ subscriptList ~ del("]") map {
      case _ ~ (sl +: Nil) ~ _ => SubscriptTrailer(sl)
      case _ ~ (slices) ~ _ => SubscriptTrailer(ExtSlice(slices))
    }
  lazy val trailerName: Syntax[AtomTrailer] = del(".") ~ nameString map { case _ ~ n => NameTrailer(n) }

  lazy val argList: Syntax[Seq[CallArg]] = argument ~ many(del(",") ~ argument) map { // ~ opt(",")
    case a1 ~ seq => a1 +: seq.map { case _ ~ a => a }
  } 

  lazy val subscriptList: Syntax[Seq[Slice]] = subscript ~ many(del(",") ~ subscript) map { // ~ opt(",")
    case s1 ~ seq => s1 +: seq.map { case _ ~ s => s }
  }

  lazy val argument: Syntax[CallArg] = argumentStartingWithTest | argumentStar | argumentDoubleStar
  lazy val argumentStartingWithTest: Syntax[CallArg] =
    test ~ opt(op(":=") ~ test | del("=") ~ test) map {
      case e1 ~ o => o match {
        case None => PosArg(e1)
        case Some(":=" ~ e2) => PosArg(NamedExpr(e1, e2))
        case Some("=" ~ e2) => KeywordArg(Some(e1), e2)
      }
    }
  lazy val argumentStar: Syntax[CallArg] = op("*") ~ test map {
    case _ ~ e => PosArg(e) // TODO is this correct ?
  }
  lazy val argumentDoubleStar: Syntax[CallArg] = op("**") ~ test map {
    case _ ~ e => KeywordArg(None, e)
  }

  lazy val subscript: Syntax[Slice] =  subscriptStartingWithTest | subscriptStartingWithColon

  // [ x:x:x ]
  lazy val subscriptStartingWithTest: Syntax[Slice] = (test ~ opt(subscriptFollows)) map {
    case t ~ None => Index(t)
    case t ~ Some((o1, o2)) => DefaultSlice(Some(t), o1, o2)
  }

  // [ :x:x ]
  lazy val subscriptStartingWithColon: Syntax[Slice] = subscriptFollows map {
    case (o1, o2) => DefaultSlice(None, o1, o2)
  }

  // [x :x:x ]
  lazy val subscriptFollows: Syntax[(Option[Expr], Option[Expr])] =
    del(":") ~ opt(test) ~ opt(sliceOp) map {
      case _ ~ o1 ~ o2 => (o1, o2.getOrElse(None))
    }

  // [x:x :x ]
  lazy val sliceOp: Syntax[Option[Expr]] = del(":") ~ opt(test) map { case _ ~ o => o }

  // either a list comprehension or a tuple
  lazy val testListComp: Syntax[(Expr, TestListCompTrailer)] =
    (namedExprTest | starExpr) ~ (compForAsTestListCompTrailer | namedOrStarListComp) map {
      case e ~ f => (e, f)
    }
  
  lazy val compForAsTestListCompTrailer: Syntax[TestListCompTrailer] =
    compFor map (CompForTrailer(_))

  lazy val compFor: Syntax[Seq[Comprehension]] = /*[async]*/
    kw("for") ~ exprList ~ kw("in") ~ orTest ~ opt(compIter) map {
      case _ ~ forExps ~ _ ~ inExp ~ optFollow => optFollow match {
        case None => Seq(Comprehension(singleExprOrTuple(forExps), inExp, Seq.empty))
        case Some((ifs, comps)) => {
          val headComp = Comprehension(singleExprOrTuple(forExps), inExp, ifs)
          headComp +: comps
        }
      }
    }
  
  lazy val compIf: Syntax[(Seq[Expr], Seq[Comprehension])] =
    kw("if") ~ testNoCond ~ opt(compIter) map {
      case _ ~ e ~ Some((ifs, comps)) => (e +: ifs, comps)
      case _ ~ e ~ None => (Seq(e), Seq.empty)
    }
  lazy val compIter: Syntax[(Seq[Expr], Seq[Comprehension])] = recursive(
    ((compFor.map (s => (Seq.empty[Expr], s))) | compIf)
  )
  // TODO optional trailing ','
  lazy val exprList: Syntax[Seq[Expr]] = (expr | starExpr) ~ many(del(",") ~ (expr | starExpr)) map {
    case e1 ~ s => e1 +: s.map{ case _ ~ e => e }
  }

  lazy val namedOrStarListComp: Syntax[TestListCompTrailer] =
    many(del(",") ~ (namedExprTest | starExpr)) map(seq => ExprSeqTrailer(seq.map{ case _ ~ e => e}))

  lazy val testListStarExpr: Syntax[Expr] = testListStarExprP1 map {
    case e +: Nil => e
    case seq => Tuple(seq)
  }

  lazy val testListStarExprP1: Syntax[Seq[Expr]] = recursive(
    (test | starExpr) ~ opt(testListStarExprP2) map {
      case t ~ tListOpt => t +: tListOpt.getOrElse(Seq.empty)
    })

  lazy val testListStarExprP2: Syntax[Seq[Expr]] =
    del(",") ~ opt(testListStarExprP1) map {
      case _ ~ tListOpt => tListOpt.getOrElse(Seq.empty)
    }

  lazy val yieldExpr: Syntax[Expr] = kw("yield") ~ opt(yieldArg | yieldFrom) map {
    case _ ~ None => Yield(None)
    case _ ~ Some(y) => y
  }

  lazy val yieldArg: Syntax[Expr] = testListStarExpr map {
    case e => Yield(Some(e))
  }

  lazy val yieldFrom: Syntax[Expr] = kw("from") ~ test map {
    case _ ~ t => YieldFrom(t)
  }
  
  def run(ctx: Context)(v: Iterator[Token]): Module = {
    if (!module.isLL1) {
      debug(module)

      println(namedExprTest.first)
      println(starExpr.first)
      println((expr | starExpr).first)
      println(test.first)

      ctx.reporter.fatal("Syntax is not LL1!")
    } else {
      println("Syntax is LL1!")
    }
    val parser = LL1(module)
    
    parser(v) match {
      case LL1.Parsed(value, rest) => value
      case LL1.UnexpectedToken(token, rest) => {
        println(token)
        println(token.position)
        ctx.reporter.fatal("Invalid token")
      }
      case LL1.UnexpectedEnd(rest) => ctx.reporter.fatal("Invalid end")
    }
  }
  
  def getKind(token: Token): Kind = token match {
    case _:BytesLiteral | _:IntLiteral | _:FloatLiteral | _:ImaginaryLiteral => NumberClass
    case _:StringLiteral => StringClass
    case Keyword(name) => KeywordClass(name)
    case Operator(op) => OperatorClass(op)
    case Identifier(name) => NameClass
    case Delimiter(del) => DelimiterClass(del)
    case Newline() => NewlineClass
    case EOF() => EOFClass
    case _ => OtherClass
  }
}