package spp.parsing

import scallion.syntactic._
import scallion.syntactic.Unfolds._

import spp.utils._
import spp.structure._
import spp.structure.AbstractSyntaxTree._
import spp.parsing._
import spp.structure._
import spp.structure.Tokens._
import spp.structure.TokenClasses._

import scala.language.implicitConversions
import spp.lexer.Lexer

object Parser extends Syntaxes with ll1.Parsing with Operators with ll1.Debug with PrettyPrinting {
  type Token = Tokens.Token
  type Kind = TokenClass

  import SafeImplicits._

  def kwU(value: String): Syntax[Unit] = elem(KeywordClass(value)).unit(Keyword(value))
  def delU(value: String): Syntax[Unit] = elem(DelimiterClass(value)).unit(Delimiter(value))
  def opU(value: String): Syntax[Unit] = elem(OperatorClass(value)).unit(Operator(value))

  def kw(value: String): Syntax[String] = accept(KeywordClass(value))({
    case _ => value
  }, {
    case `value` => Seq(Keyword(value))
    case _ => Seq()
  })


  def del(value: String): Syntax[String] = accept(DelimiterClass(value))({
    case _ => value
  }, {
    case `value` => Seq(Delimiter(value))
    case _ => Seq()
  })


  def op(value: String): Syntax[String] = accept(OperatorClass(value))({
    case _ => value
  }, {
    case `value` => Seq(Operator(value))
    case _ => Seq()
  })

  def compKw(value1: String, value2: String): Syntax[String] = kw(value1) ~ kw(value2) map ({
    case v1 ~ v2 => v1 ++ " " ++ v2
  }, {
    case _ => Seq() // TODO
    case _ => Seq(value1 ~ value2)
  })

  // helper function
  def singleExprOrTuple(exps: Seq[Expr]) = exps match {
    case Seq() => Tuple(Seq.empty)
    case Seq(head) => head
    case seq => Tuple(seq)
  }

  lazy val nameString: Syntax[String] = accept(NameClass) ({
    case Identifier(name) => name
  }, {
    case name => Seq(Identifier(name))
  })

  lazy val name: Syntax[Expr] = nameString map ({
    case n => Name(n)
  }, {
    case Name(n) => Seq(n)
    case _ => Seq()
  })

  lazy val newLine: Syntax[Unit] = elem(NewlineClass).unit(Newline())
  lazy val indent: Syntax[Unit] = elem(IndentClass).unit(Indent())
  lazy val dedent: Syntax[Unit] = elem(DedentClass).unit(Dedent())
  lazy val eof: Syntax[Unit] = elem(EOFClass).unit(EOF())

  lazy val number: Syntax[Expr] = accept(NumberClass) ({
    case IntLiteral(v) => IntConstant(v)
    case FloatLiteral(v) => FloatConstant(v)
    case ImaginaryLiteral(v) => ImaginaryConstant(v)
  }, {
    case IntConstant(v) => Seq(IntLiteral(v))
    case FloatConstant(v) => Seq(FloatLiteral(v))
    case ImaginaryConstant(v) => Seq(ImaginaryLiteral(v))
    case _ => Seq()
  })

  lazy val module: Syntax[Module] = (many(stmt) ~ eof.skip) map ({ // TODO doc says there could be NEWLINE only ??
    case seq => Module(seq.reduceLeft(_ ++ _))
  }, {
    case Module(statements) =>{
      val res = Seq(statements.map(Seq(_)))
      res
    }
    case _ => Seq()
  })

  lazy val stmt: Syntax[Seq[Statement]] = simpleStmt /*| compoundStmt.map(st => Seq(st))*/

  // One or more smallStmt on a single line
  lazy val simpleStmt: Syntax[Seq[Statement]] =
    rep1sep(smallStmt, delU(";")) ~ newLine.skip

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

  lazy val exprStmt: Syntax[Statement] =
    testListStarExpr ~ many(delU("=").skip ~ testListStarExpr) map ({
      case e ~ Seq() => ExprStmt(e)
      case e ~ seq => Assign(e +: seq.init, seq.last)
    }, {
      case ExprStmt(e) => Seq(e ~ Seq.empty)
      case Assign(targets, value) => Seq(targets.head ~ (targets.tail :+ value))
      case _ => Seq()
    })
  /*
  lazy val delStmt: Syntax[Statement] = ???
  lazy val passStmt: Syntax[Statement] = ???
  lazy val flowStmt: Syntax[Statement] = ???
  lazy val importStmt: Syntax[Statement] = ???
  lazy val globalStmt: Syntax[Statement] = ???
  lazy val nonLocalStmt: Syntax[Statement] = ???
  lazy val assertStmt: Syntax[Statement] = ???*/
  lazy val yieldStmt: Syntax[ExprStmt] = yieldExpr map ({
    case yieldExp => ExprStmt(yieldExp)
  }, {
    case ExprStmt(yieldExp) => Seq(yieldExp)
    case _ => Seq()
  })
  
  // What follows ':' (example after an if ...: or for ...:)
  lazy val suiteStmt: Syntax[Seq[Statement]] =
    simpleStmt | (newLine.skip ~ indent.skip ~ many1(stmt) ~ dedent.skip).map ({
      case seqSeqStmts => seqSeqStmts.reduce(_ ++ _)
    }, {
      case statements => Seq(statements.map(Seq(_)))
    })

  lazy val namedExprTest: Syntax[Expr] = test ~ opt(opU(":=").skip ~ test) map ({
    case t ~ None => t
    case t1 ~ Some(t2) => NamedExpr(t1, t2)
  }, {
    case NamedExpr(t1, t2) => Seq(t1 ~ Some(t2))
    case e => Seq(e ~ None)
  })

  lazy val test: Syntax[Expr] = recursive(
    orTest ~ opt(kwU("if").skip ~ orTest ~ kwU("else").skip ~ test) /* lambdef*/ map ({
      case vTrue ~ Some(condition ~ vFalse) => IfExpr(condition, vTrue, vFalse)
      case v ~ None => v
    }, {
      case IfExpr(condition, vTrue, vFalse) => Seq(vTrue ~ Some(condition ~ vFalse))
      case e => Seq(e ~ None)
    })
  )

  lazy val testNoCond: Syntax[Expr] = orTest /*lambdef_no_cond*/

  lazy val orTest: Syntax[Expr] = operators(notTest)(
    kw("or") is LeftAssociative,
    kw("and") is LeftAssociative
  ) ({
    case (l, op, r) => BoolOp(op, l, r)
  }, {
    case BoolOp(op, l, r) => (l, op, r)
  })

  lazy val notTest: Syntax[Expr] = comparison | recursive(
    kwU("not").skip ~ notTest map ({
      case e => UnaryOp("not", e)
    }, {
      case UnaryOp(op, e) => Seq(e)
      case _ => Seq()
    })
  )

  // comparisons can be chained : 1 < 2 < x == 3
  lazy val comparison: Syntax[Expr] = expr ~ many(comparisonOp ~ expr) map ({
    case e1 ~ Seq() => e1
    case e1 ~ l => Compare(e1, l.map { case op ~ e => op }, l.map { case op ~ e => e } )
  }, {
    case Compare(left, ops, comparators) => Seq(left ~ (ops zip comparators).map {
      case (op, comp) => op ~ comp
    })
    case e => Seq(e ~ Seq.empty)
  })

  lazy val comparisonOp = oneOf(
    op("<"), op(">"), op("=="), op(">="), op("<="), op("<>"), op("!="),
    kw("in"), compKw("not", "in"), isOperator
  )

  // Special case for LL1
  lazy val isOperator: Syntax[String] = kwU("is").skip ~ opt(kw("not")) map ({
    case Some(_) => "is not"
    case None => "is"
  }, {
    case "is not" => Seq(Some("not"))
    case "is" => Seq(None)
    case _ => Seq()
  })

  lazy val starExpr: Syntax[Expr] = opU("*").skip ~ expr map ({
    case e => Starred(e)
  }, {
    case Starred(e) => Seq(e)
    case _ => Seq()
  })

  lazy val expr: Syntax[Expr] = operators(atomExpr)(
    op("|") is LeftAssociative,
    op("^") is LeftAssociative,
    op("&") is LeftAssociative,
    op("<<") | op(">>") is LeftAssociative,
    op("*") | op("@") | op("/") | op("%") | op("//") is LeftAssociative,
    op("+") | op("-") | op("~") is LeftAssociative,
    op("**") is RightAssociative
  )({
    case (l, op, r) => BinOp(op, l, r)
  }, {
    case BinOp(op, l, r) => (l, op, r)
  })

  lazy val atomExpr: Syntax[Expr] = /* await */ atom ~ (many(trailer)) map ({
    case e ~ Seq() => e
    case e ~ trailers => trailers.foldLeft(e)((previousExp, trailer) => trailer match {
      case Left(name) => Attribute(previousExp, name)
      case Right(either) => either match {
        case Left(args) => Call(previousExp, args)
        case Right(slice) => Subscript(previousExp, slice)
      } 
    })
  }, (e => e match {
    case _:Attribute | _:Call | _:Subscript =>
      unfoldLeft[Either[String, Either[Seq[CallArg], Slice]], Expr]{
        case Attribute(value, attr) => (value, Left(attr))
        case Call(func, args) => (func, Right(Left(args)))
        case Subscript(value, slice) => (value, Right(Right(slice)))
      }(e)
    case _ => Seq(e ~ Seq.empty) // expression without trailer
  }))

  // TODO comprehensions, ellipsis and strings
  lazy val atom: Syntax[Expr] =
    recursive (name | number | atomPredef | atomParens | atomBrackets)

  // parenthesized expression
  lazy val atomParens: Syntax[Expr] = // TODO centralize tuple creation
    delU("(").skip ~ opt(yieldExpr | atomParensContent) ~ delU(")").skip map ({
      case None => Tuple(Seq.empty) // empty parenthesis is the 0-tuple
      case Some(e) => e
    }, {
      case Tuple(Seq()) => Seq(None)
      case e => Seq(Some(e))
    })
  lazy val atomParensContent: Syntax[Expr] = testListComp map ({
    case Left((e, comps)) => GeneratorExp(e, comps)
    case Right(exprs) => singleExprOrTuple(exprs)
  }, {
    case GeneratorExp(e, comps) => Seq(Left((e, comps)))
    case Tuple(exprs) => Seq(Right(exprs))
    case e => Seq(Right(Seq(e)))
  })

  // bracketized expression, either a list literal or a list comprehension
  lazy val atomBrackets: Syntax[Expr] =
    delU("[").skip ~ opt(atomBracketsContent) ~ delU("]").skip map ({
      case None => List(Seq.empty)
      case Some(e) => e
    }, {
      case List(Seq()) => Seq(None)
      case e => Seq(Some(e))
    })
  lazy val atomBracketsContent: Syntax[Expr] = testListComp map ({
    case Left((e, comps)) => ListComp(e, comps)
    case Right(exprs) => List(exprs)
  }, {
    case ListComp(e, comps) => Seq(Left((e, comps)))
    case List(exprs) => Seq(Right(exprs))
    case _ => Seq()
  })
  
  // None, True, False
  lazy val atomPredef: Syntax[Expr] = (kw("None") | kw("True") | kw("False")) map ({
    case v => Name(v)
  }, {
    case Name(v) => Seq(v)
    case _ => Seq()
  })
  
  // something immediately after an atomic expression
  lazy val trailer: Syntax[Either[String, Either[Seq[CallArg], Slice]]] =
    trailerName || (trailerCall || trailerSubscript)

  lazy val trailerCall: Syntax[Seq[CallArg]] =
    delU("(").skip ~ argList ~ delU(")").skip
  lazy val argList: Syntax[Seq[CallArg]] = repsep(argument, delU(","))

  lazy val trailerSubscript: Syntax[Slice] =
    delU("[").skip ~ subscriptList ~ delU("]").skip map ({
      case sl +: Seq() => sl
      case slices => ExtSlice(slices)
    }, {
      case ExtSlice(slices) => Seq(slices)
      case sl => Seq(Seq(sl))
    })
  lazy val subscriptList: Syntax[Seq[Slice]] = rep1sep(subscript, delU(","))

  lazy val trailerName: Syntax[String] = delU(".").skip ~ nameString

  lazy val argument: Syntax[CallArg] = argumentStartingWithTest | argumentStar | argumentDoubleStar
  lazy val argumentStartingWithTest: Syntax[CallArg] =
    test ~ opt(opU(":=").skip ~ test || delU("=").skip ~ test) map ({
      case e1 ~ o => o match {
        case None => PosArg(e1)
        case Some(Left(e2)) => PosArg(NamedExpr(e1, e2))
        case Some(Right(e2)) => KeywordArg(Some(e1), e2)
      }
    }, {
      case KeywordArg(Some(e1), e2) => Seq(e1 ~ Some(Right(e2)))
      case PosArg(NamedExpr(e1, e2)) => Seq(e1 ~ Some(Left(e2)))
      case PosArg(e1) => Seq(e1 ~ None)
      case _ => Seq()
    })

  lazy val argumentStar: Syntax[CallArg] = opU("*").skip ~ test map ({
    case e => PosArg(e) // TODO is this correct ?
  }, {
    case PosArg(e) => Seq(e)
    case _ => Seq()
  })
  lazy val argumentDoubleStar: Syntax[CallArg] = opU("**").skip ~ test map ({
    case e => KeywordArg(None, e)
  }, {
    case KeywordArg(None, e) => Seq(e)
    case _ => Seq()
  })

  lazy val subscript: Syntax[Slice] =  subscriptStartingWithTest | subscriptStartingWithColon

  // [ x:x:x ]
  lazy val subscriptStartingWithTest: Syntax[Slice] = (test ~ opt(subscriptFollows)) map ({
    case t ~ None => Index(t)
    case t ~ Some((o1, o2)) => DefaultSlice(Some(t), o1, o2)
  }, {
    case Index(t) => Seq(t ~ None)
    case DefaultSlice(Some(t), o1, o2) => Seq(t ~ Some((o1, o2)))
    case _ => Seq()
  })

  // [ :x:x ]
  lazy val subscriptStartingWithColon: Syntax[Slice] = subscriptFollows map ({
    case (o1, o2) => DefaultSlice(None, o1, o2)
  }, {
    case DefaultSlice(None, o1, o2) => Seq((o1, o2))
    case _ => Seq()
  })

  // [x :x:x ]
  lazy val subscriptFollows: Syntax[(Option[Expr], Option[Expr])] =
    delU(":").skip ~ opt(test) ~ opt(sliceOp) map ({
      case o1 ~ o2 => (o1, o2.getOrElse(None))
    }, {
      case (o1, o2) => Seq(o1 ~ Some(o2)) // TODO check
      case _ => Seq()
    })

  // [x:x :x ]
  lazy val sliceOp: Syntax[Option[Expr]] = delU(":").skip ~ opt(test)

  // either a list comprehension or a tuple
  lazy val testListComp: Syntax[Either[(Expr, Seq[Comprehension]), Seq[Expr]]] =
    (namedExprTest | starExpr) ~ (compFor || namedOrStarListComp) map ({
      case e ~ Left(comps) => Left(e, comps)
      case e ~ Right(exprs) => Right(e +: exprs)
    }, {
      case Left((e, comps)) => Seq(e ~ Left(comps))
      case Right(e +: exprs) => Seq(e ~ Right(exprs))
      case _ => Seq()
    })
  
  lazy val compFor: Syntax[Seq[Comprehension]] = /*[async]*/
    kwU("for").skip ~ exprList ~ kwU("in").skip ~ orTest ~ opt(compIter) map ({
      case forExps ~ inExp ~ optFollow => optFollow match {
        case None => Seq(Comprehension(singleExprOrTuple(forExps), inExp, Seq.empty))
        case Some((ifs, comps)) => {
          val headComp = Comprehension(singleExprOrTuple(forExps), inExp, ifs)
          headComp +: comps
        }
      }
    }, {
      case Seq(Comprehension(target, iter, Seq())) => Seq(Seq(target) ~ iter ~ None)
      case Comprehension(target, iter, ifs) :: tail => Seq(Seq(target) ~ iter ~ Some(ifs, tail))
      case _ => Seq()
    })
  
  lazy val compIf: Syntax[(Seq[Expr], Seq[Comprehension])] =
    kwU("if").skip ~ testNoCond ~ opt(compIter) map ({
      case e ~ Some((ifs, comps)) => (e +: ifs, comps)
      case e ~ None => (Seq(e), Seq.empty)
    }, {
      case (Seq(e), Seq()) => Seq(e ~ None)
      case (e +: ifs, comps) => Seq(e ~ Some((ifs, comps)))
      case _ => Seq()
    })
  lazy val compIter: Syntax[(Seq[Expr], Seq[Comprehension])] = recursive(
    compIterFor | compIf
  )

  lazy val compIterFor: Syntax[(Seq[Expr], Seq[Comprehension])] = compFor map ({
    case s => (Seq.empty, s)
  }, {
    case (Seq(), s) => Seq(s)
    case _ => Seq()
  })
  // TODO optional trailing ','
  lazy val exprList: Syntax[Seq[Expr]] = rep1sep((expr | starExpr), delU(","))

  lazy val namedOrStarListComp: Syntax[Seq[Expr]] =
    many(delU(",").skip ~ (namedExprTest | starExpr))

  lazy val testListStarExpr: Syntax[Expr] =
    rep1sep(test | starExpr, delU(",")) /*~ opt(del(","))*/ map ({
      case Seq(tail) /*~ None*/ => tail
      case seq /*~ _*/ => Tuple(seq)
    }, {
      case Tuple(seq) => Seq(seq)
      case e => Seq(Seq(e))
    })
  
  // TODO yield trait ?
  lazy val yieldExpr: Syntax[Expr] =
    kwU("yield").skip ~ opt(testListStarExpr || kwU("from").skip ~ test) map ({
      case None => Yield(None)
      case Some(Left(e)) => Yield(Some(e))
      case Some(Right(e)) => YieldFrom(e)
    }, {
      case Yield(None) => Seq(None)
      case Yield(Some(e)) => Seq(Some(Left(e)))
      case YieldFrom(e) => Seq(Some(Right(e)))
      case _ => Seq()
    })

  val parser = LL1(module)
  val printer = PrettyPrinter(module)
  
  def apply(ctx: Context, tokens: Iterator[Token]): Module = {
    if (!module.isLL1) {
      debug(module)

      ctx.reporter.fatal("Syntax is not LL1!")
    } else {
      println("Syntax is LL1!")
    }
    
    val res = parser(tokens) match {
      case LL1.Parsed(value, rest) => value
      case LL1.UnexpectedToken(token, rest) => {
        println(token)
        println(token.position)
        ctx.reporter.fatal("Invalid token")
      }
      case LL1.UnexpectedEnd(rest) => ctx.reporter.fatal("Invalid end")
    }

    println(res)

    println("Pretty printing: ")
    val pretty = unapply(res).get
    println(pretty)

    res
  }

  def unapply(value: Module): Option[String] ={
    val it = printer(value).take(3).toList
    it.foreach(println(_))
    it.map(Lexer.unapply(_)).toList.headOption

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