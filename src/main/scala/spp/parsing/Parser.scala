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

  import Implicits._

  implicit def classToSyntax(k: Kind): Syntax[Token] = elem(k)

  def kw(value: String): Syntax[String] = accept(KeywordClass(value))({
    case _ => value
  }, {
    case value => Seq(Keyword(value))
  })

  def del(value: String): Syntax[String] = accept(DelimiterClass(value))({
    case _ => value
  }, {
    case value => Seq(Delimiter(value))
  })

  def op(value: String): Syntax[String] = accept(OperatorClass(value))({
    case _ => value
  }, {
    case value => Seq(Operator(value))
  })

  def compKw(value1: String, value2: String): Syntax[String] = kw(value1) ~ kw(value2) map ({
    case v1 ~ v2 => v1 ++ " " ++ v2
  }, {
    case _ => ??? // TODO
  })

  // helper function
  def singleExprOrTuple(exps: Seq[Expr]) = exps match {
    case Nil => Tuple(Seq.empty)
    case head :: Nil => head
    case seq => Tuple(seq)
  }

  lazy val nameString: Syntax[String] = accept(NameClass){ case Identifier(name) => name }

  lazy val name: Syntax[Expr] = nameString map (n => Name(n))

  lazy val newLine: Syntax[Unit] = elem(NewlineClass).unit(Newline())
  lazy val indent: Syntax[Unit] = elem(IndentClass).unit(Indent())
  lazy val dedent: Syntax[Unit] = elem(DedentClass).unit(Dedent())

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
    rep1sep(smallStmt, del(";")) ~ newLine.skip

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

  lazy val exprStmt: Syntax[Statement] = (testListStarExpr ~ many1(del("=").skip ~ testListStarExpr)) map {
    case seqExpr ~ seqSeqExpr => {
      Assign(seqExpr +: seqSeqExpr.init, seqSeqExpr.last)
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
    simpleStmt | (newLine.skip ~ indent.skip ~ many1(stmt) ~ dedent.skip).map {
      case seqSeqStmts => seqSeqStmts.reduce(_ ++ _)
    }

  lazy val namedExprTest: Syntax[Expr] = test ~ opt(op(":=").skip ~ test) map {
    case t ~ None => t
    case t1 ~ Some(t2) => NamedExpr(t1, t2)
  }

  // lazy val namedExprTest: Syntax[Expr] = ???
  lazy val test: Syntax[Expr] = recursive(
    orTest ~ opt(kw("if").skip ~ orTest ~ kw("else").skip ~ test) /* lambdef*/ map {
      case vTrue ~ Some(condition ~ vFalse) => IfExpr(condition, vTrue, vFalse)
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
    kw("not").skip ~ notTest map {
      case e => UnaryOp("not", e)
    }
  ).up[Expr] | comparison

  // comparisons can be chained : 1 < 2 < x == 3
  lazy val comparison: Syntax[Expr] = expr ~ many(comparisonOp ~ expr) map {
    case e1 ~ Nil => e1
    case e1 ~ l => Compare(e1, l.map { case op ~ e => op }, l.map { case op ~ e => e } )
  }

  lazy val comparisonOp = oneOf(
    op("<"), op(">"), op("=="), op(">="), op("<="), op("<>"), op("!="),
    kw("in"), compKw("not", "in"), isOperator
  )

  // Special case for LL1
  lazy val isOperator = kw("is").skip ~ opt(kw("not")) map {
    case Some(_) => "is not"
    case None => "is"
  }

  lazy val starExpr: Syntax[Expr] = op("*").skip ~ expr map { case e => Starred(e) }

  lazy val expr: Syntax[Expr] = operators(atomExpr)(
    op("|") is LeftAssociative,
    op("^") is LeftAssociative,
    op("&") is LeftAssociative,
    op("<<") | op(">>") is LeftAssociative,
    op("*") | op("@") | op("/") | op("%") | op("//") is LeftAssociative,
    op("+") | op("-") | op("~") is LeftAssociative,
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
      case Left(name) => Attribute(previousExp, name)
      case Right(either) => either match {
        case Left(args) => Call(previousExp, args)
        case Right(slice) => Subscript(previousExp, slice)
      } 
    })
  }

  // TODO comprehensions, ellipsis and strings
  lazy val atom: Syntax[Expr] =
    recursive (name | number | atomPredef | atomParens | atomBrackets)

  // parenthesized expression
  lazy val atomParens: Syntax[Expr] =
    del("(").skip ~ opt(yieldExpr | atomParensContent) ~ del(")").skip map {
      case None => Tuple(Seq.empty) // empty parenthesis is the 0-tuple
      case Some(e) => e
    }
  lazy val atomParensContent: Syntax[Expr] = testListComp map {
    case Left((e, comps)) => GeneratorExp(e, comps)
    case Right(exprs) => singleExprOrTuple(exprs)
  }

  // bracketized expression, either a list literal or a list comprehension
  lazy val atomBrackets: Syntax[Expr] =
    del("[").skip ~ opt(atomBracketsContent) ~ del("]").skip map {
      case None => List(Seq.empty)
      case Some(e) => e
    }
  lazy val atomBracketsContent: Syntax[Expr] = testListComp map {
    case Left((e, comps)) => ListComp(e, comps)
    case Right(exprs) => List(exprs)
  }
  
  // None, True, False
  lazy val atomPredef: Syntax[Expr] = (kw("None") | kw("True") | kw("False")) map {
    case v => Name(v)
  }
  
  // something immediately after an atomic expression
  lazy val trailer: Syntax[Either[String, Either[Seq[CallArg], Slice]]] =
    trailerName || (trailerCall || trailerSubscript)

  lazy val trailerCall: Syntax[Seq[CallArg]] =
    del("(").skip ~ argList ~ del(")").skip
  lazy val argList: Syntax[Seq[CallArg]] = repsep(argument, del(","))

  lazy val trailerSubscript: Syntax[Slice] =
    del("[").skip ~ subscriptList ~ del("]").skip map {
      case (sl +: Nil) => sl
      case (slices) => ExtSlice(slices)
    }
  lazy val subscriptList: Syntax[Seq[Slice]] = rep1sep(subscript, del(","))

  lazy val trailerName: Syntax[String] = del(".").skip ~ nameString

  lazy val argument: Syntax[CallArg] = argumentStartingWithTest | argumentStar | argumentDoubleStar
  lazy val argumentStartingWithTest: Syntax[CallArg] =
    test ~ opt(op(":=").skip ~ test || del("=").skip ~ test) map {
      case e1 ~ o => o match {
        case None => PosArg(e1)
        case Some(Left(e2)) => PosArg(NamedExpr(e1, e2))
        case Some(Right(e2)) => KeywordArg(Some(e1), e2)
      }
    }

  lazy val argumentStar: Syntax[CallArg] = op("*").skip ~ test map {
    case e => PosArg(e) // TODO is this correct ?
  }
  lazy val argumentDoubleStar: Syntax[CallArg] = op("**").skip ~ test map {
    case e => KeywordArg(None, e)
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
    del(":").skip ~ opt(test) ~ opt(sliceOp) map {
      case o1 ~ o2 => (o1, o2.getOrElse(None))
    }

  // [x:x :x ]
  lazy val sliceOp: Syntax[Option[Expr]] = del(":").skip ~ opt(test)

  // either a list comprehension or a tuple
  lazy val testListComp: Syntax[Either[(Expr, Seq[Comprehension]), Seq[Expr]]] =
    (namedExprTest | starExpr) ~ (compFor || namedOrStarListComp) map {
      case e ~ Left(comps) => Left(e, comps)
      case e ~ Right(exprs) => Right(e +: exprs)
    }
  
  lazy val compFor: Syntax[Seq[Comprehension]] = /*[async]*/
    kw("for").skip ~ exprList ~ kw("in").skip ~ orTest ~ opt(compIter) map {
      case forExps ~ inExp ~ optFollow => optFollow match {
        case None => Seq(Comprehension(singleExprOrTuple(forExps), inExp, Seq.empty))
        case Some((ifs, comps)) => {
          val headComp = Comprehension(singleExprOrTuple(forExps), inExp, ifs)
          headComp +: comps
        }
      }
    }
  
  lazy val compIf: Syntax[(Seq[Expr], Seq[Comprehension])] =
    kw("if").skip ~ testNoCond ~ opt(compIter) map {
      case e ~ Some((ifs, comps)) => (e +: ifs, comps)
      case e ~ None => (Seq(e), Seq.empty)
    }
  lazy val compIter: Syntax[(Seq[Expr], Seq[Comprehension])] = recursive(
    ((compFor.map (s => (Seq.empty[Expr], s))) | compIf)
  )
  // TODO optional trailing ','
  lazy val exprList: Syntax[Seq[Expr]] = rep1sep((expr | starExpr), del(","))

  lazy val namedOrStarListComp: Syntax[Seq[Expr]] =
    many(del(",").skip ~ (namedExprTest | starExpr))

  lazy val testListStarExpr: Syntax[Expr] = 
    rep1sep(test | starExpr, del(",")) /*~ opt(del(","))*/ map {
      case (tail :: Nil) /*~ None*/ => tail
      case seq /*~ _*/ => Tuple(seq)
    }
  
  // TODO yield trait ?
  lazy val yieldExpr: Syntax[Expr] =
    kw("yield").skip ~ opt(testListStarExpr || kw("from").skip ~ test) map {
      case None => Yield(None)
      case Some(Left(e)) => Yield(Some(e))
      case Some(Right(e)) => YieldFrom(e)
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