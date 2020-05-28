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

  def followsTuple[A, B](s1: Syntax[A], s2: Syntax[B]): Syntax[(A, B)] = s1 ~ s2 map ({
    case a ~ b => (a, b)
  }, {
    case (a, b) => Seq(a ~ b)
  })

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

  lazy val string: Syntax[Expr] = accept(StringClass) ({
    case sl:StringLiteral => StringLiteralParser.parse(sl)
  }, {
    case StringConstant(value) => Seq(StringLiteral("", "'", value))
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

  lazy val stmt: Syntax[Seq[Statement]] =
    simpleStmt | (recursive(compoundStmt) map({
      case st => Seq(st)
    }, {
      case Seq(st) => Seq(st)
      case _ => Seq()
    }))

  // One or more smallStmt on a single line
  lazy val simpleStmt: Syntax[Seq[Statement]] =
    rep1sep(smallStmt, delU(";")) ~ newLine.skip

  lazy val compoundStmt: Syntax[Statement] = ifStmt | whileStmt | forStmt | tryStmt |
    withStmt | funcDef | classDef /* | decorated | asyncStmt*/

  def optSuite(keyword: String): Syntax[Seq[Statement]] =
    opt(kwU(keyword).skip ~ delU(":").skip ~ suiteStmt) map ({
      case optStatements => optStatements.getOrElse(Seq())
    }, {
      case Seq() => Seq(None)
      case statements => Seq(Some(statements))
    })

  lazy val ifStmt: Syntax[Statement] =
    ((kwU("if").skip ~ ifCondThen) +:
    many(kwU("elif").skip ~ ifCondThen)) ~
    optSuite("else") map ({
      case ifs ~ elze => {
        val (lastCond, lastCondBody) = ifs.last
        val lastIf = If(lastCond, lastCondBody, elze)

        ifs.init.foldRight(lastIf){
          case ((cond, body), accIf) => If(cond, body, Seq(accIf))
        }
      }
    }, {
      case iff:If => unfoldRight[(Expr, Seq[Statement]), Seq[Statement]] {
        case Seq(If(cond, body, elze)) => ((cond, body), elze)
      }(Seq(iff))
      case _ => Seq()
    })
  
  // A condition (after if or elif) followed by statements
  lazy val ifCondThen: Syntax[(Expr, Seq[Statement])] =
    followsTuple(namedExprTest, delU(":").skip ~ suiteStmt)
  
  lazy val whileStmt: Syntax[Statement] =
    kwU("while").skip ~ namedExprTest ~ delU(":").skip ~ suiteStmt ~
    optSuite("else") map ({
      case cond ~ statements ~ elze => While(cond, statements, elze)
    }, {
      case While(cond, statements, elze) => Seq(cond ~ statements ~ elze)
    })

  lazy val forStmt: Syntax[Statement] =
    kwU("for").skip ~ exprList ~ kwU("in").skip ~ testList ~ delU(":").skip ~ suiteStmt ~
    optSuite("else") map ({
      case target ~ iter ~ body ~ orElse =>
        For(singleExprOrTuple(target), iter, body, orElse)
    }, {
      case For(Tuple(seq), iter, body, orElse) => Seq(seq ~ iter ~ body ~ orElse)
      case For(target, iter, body, orElse) => Seq(Seq(target) ~ iter ~ body ~ orElse)
    })

  lazy val tryStmt: Syntax[Statement] =
    kwU("try").skip ~ delU(":").skip ~ suiteStmt ~
    (
      many1(except) ~ optSuite("else") ~ optSuite("finally") ||
      kwU("finally").skip ~ delU(":").skip ~ suiteStmt
    ) map ({
      case body ~ Left(handlers ~ orelse ~ finalbody) => Try(body, handlers, orelse, finalbody)
      case body ~ Right(finalbody) => Try(body, Seq(), Seq(), finalbody)
    }, {
      case Try(body, Seq(), Seq(), finalbody) =>
        Seq(body ~ Right(finalbody))
      case Try(body, handlers, orelse, finalbody) =>
        Seq(body ~ Left(handlers ~ orelse ~ finalbody))
    })

  lazy val except: Syntax[ExceptionHandler] =
    kwU("except").skip ~ opt(test ~ opt(kwU("as").skip ~ nameString)) ~
    delU(":").skip ~ suiteStmt map ({
      case None ~ body => ExceptionHandler(None, None, body)
      case Some(tpe ~ optName) ~ body => ExceptionHandler(Some(tpe), optName, body)
    }, {
      case ExceptionHandler(None, None, body) => Seq(None ~ body)
      case ExceptionHandler(Some(tpe), optName, body) => Seq(Some(tpe ~ optName) ~ body)
      case _ => Seq()
    })

  lazy val withStmt: Syntax[Statement] =
    kwU("with").skip ~ rep1sep(withItem, delU(",")) ~ delU(":").skip ~ suiteStmt map ({
      case items ~ suite => With(items, suite)
    }, {
      case With(items, suite) => Seq(items ~ suite)
      case _ => Seq()
    })

  lazy val withItem: Syntax[WithItem] =
    test ~ opt(kwU("as").skip ~ expr) map ({
      case item ~ optName => WithItem(item, optName)
    }, {
      case WithItem(item, optName) => Seq(item ~ optName)
      case _ => Seq()
    })

  lazy val funcDef: Syntax[Statement] =
    kwU("def").skip ~ nameString ~ parameters ~ opt(delU("->").skip ~ test) ~
    delU(":").skip ~ suiteStmt map ({
      case name ~ params ~ optReturnType ~ body => FunctionDef(name, params, body, Seq(), optReturnType)
    }, {
      case FunctionDef(name, params, body, Seq(), optReturnType) =>
        Seq(name ~ params ~ optReturnType ~ body)
      case _ => Seq()
    })
  
  lazy val parameters: Syntax[Arguments] =
    delU("(").skip ~ typedArgsList ~ delU(")").skip map ({
      case (args, varargs, kwonly, kwargs) => Arguments(args, varargs, kwonly, kwargs)
    }, {
      case Arguments(args, varargs, kwonly, kwargs) => Seq((args, varargs, kwonly, kwargs))
      case _ => Seq()
    })

  lazy val typedArgsList: Syntax[(Seq[Arg], Option[Arg], Seq[Arg], Option[Arg])] =
    repsep(tfpdefDefault, delU(",")) map ({
      case args => (args, None, Seq(), None)
    }, {
      case (args, None, Seq(), None) => Seq(args)
      case _ => Seq()
    })

  lazy val tfpdefDefault: Syntax[Arg] =
    nameString ~ opt(delU(":").skip ~ test) ~ opt(delU("=").skip ~ test) map ({
      case name ~ optAnn ~ optDft => Arg(name, optAnn, optDft)
    }, {
      case Arg(name, optAnn, optDft) => Seq(name ~ optAnn ~ optDft)
      case _ => Seq()
    })

  lazy val classDef: Syntax[Statement] =
    kwU("class").skip ~ nameString ~ opt(delU("(").skip ~ argList ~ delU(")").skip) ~
    delU(":").skip ~ suiteStmt map ({
      case name ~ Some(bases) ~ body => ClassDef(name, bases, body, Seq())
      case name ~ None ~ body => ClassDef(name, Seq(), body, Seq())
    }, {
      case ClassDef(name, Seq(), body, Seq()) => Seq(name ~ None ~ body)
      case ClassDef(name, bases, body, Seq()) => Seq(name ~ Some(bases) ~ body)
      case _ => Seq()
    })
  lazy val decorated: Syntax[Statement] = ???
  lazy val asyncStmt: Syntax[Statement] = ???

  lazy val smallStmt: Syntax[Statement] =
    exprStmt | delStmt | passStmt | flowStmt | importStmt |
    globalStmt | nonLocalStmt | assertStmt

  lazy val exprStmt: Syntax[Statement] =
    testListStarExpr ~ opt(
      many1(delU("=").skip ~ (yieldExpr | testListStarExpr)) ||
      augAssignOp ~ (yieldExpr | testList) ||
      annAssign
    ) map ({
      case e ~ None => ExprStmt(e)
      case e ~ Some(Left(Left(seq))) => Assign(e +: seq.init, seq.last)
      case e ~ Some(Left(Right(op ~ e2))) => AugAssign(e, op, e2)
      case e ~ Some(Right((ann, opt))) => AnnAssign(e, ann, opt)
    }, {
      case ExprStmt(e) => Seq(e ~ None)
      case Assign(targets, value) => Seq(targets.head ~ Some(Left(Left(targets.tail :+ value))))
      case AugAssign(e, op, e2) => Seq(e ~ Some(Left(Right(op ~ e2))))
      case AnnAssign(e, ann, opt, _) => Seq(e ~ Some(Right(ann, opt)))
      case _ => Seq()
    })
  
  lazy val annAssign: Syntax[(Expr, Option[Expr])] =
    delU(":").skip ~ test ~ opt(delU("=").skip ~ (yieldExpr | testListStarExpr)) map ({
      case t ~ opt => (t, opt)
    }, {
      case (t, opt) => Seq(t ~ opt)
    })

  lazy val augAssignOp: Syntax[String] = oneOf(
    del("+="), del("-="), del("*="), del("@="), del("/="), del("%="), del("&="),
    del("|="), del("^="), del("<<="), del(">>="), del("**="), del("//=")
  )
    
  lazy val delStmt: Syntax[Statement] = kwU("del").skip ~ exprList map ({
    case exps => Delete(exps)
  }, {
    case Delete(exps) => Seq(exps)
    case _ => Seq()
  })

  lazy val passStmt: Syntax[Statement] = kw("pass") map ({
    case pass => Pass
  }, {
    case Pass => Seq("pass")
    case _ => Seq()
  })

  lazy val flowStmt: Syntax[Statement] =
    breakStmt | continueStmt | returnStmt | raiseStmt | yieldStmt
  lazy val globalStmt: Syntax[Statement] =
    kwU("global").skip ~ rep1sep(nameString, delU(",")) map ({
      case names => Global(names)
    }, {
      case Global(names) => Seq(names)
      case _ => Seq()
    })
  lazy val nonLocalStmt: Syntax[Statement] =
    kwU("nonlocal").skip ~ rep1sep(nameString, delU(",")) map ({
      case names => Nonlocal(names)
    }, {
      case Nonlocal(names) => Seq(names)
      case _ => Seq()
    })
  lazy val assertStmt: Syntax[Statement] =
    kwU("assert").skip ~ test ~ opt(delU(",").skip ~ test) map ({
      case t ~ opt => Assert(t, opt)
    }, {
      case Assert(t, opt) => Seq(t ~ opt)
      case _ => Seq()
    })
  lazy val importStmt: Syntax[Statement] = importName | importFrom
  lazy val importName: Syntax[Statement] = kwU("import").skip ~ dottedAsNames map ({
    case aliases => Import(aliases)
  }, {
    case Import(aliases) => Seq(aliases)
    case _ => Seq()
  })

  lazy val importFrom: Syntax[Statement] =
    kwU("from").skip ~ (importFromRelative || dottedName) ~
    kwU("import").skip ~ (importStar | delU("(").skip ~ asNames ~ delU(")").skip | asNames ) map ({
      case Left((level, moduleOpt)) ~ names => ImportFrom(moduleOpt, names, Some(level))
      case Right(module) ~ names => ImportFrom(Some(module), names, None)
    }, {
      case ImportFrom(moduleOpt, names, Some(level)) => Seq(Left((level, moduleOpt)) ~ names)
      case ImportFrom(Some(module), names, None) => Seq(Right(module) ~ names)
      case _ => Seq()
    })

  lazy val importFromRelative: Syntax[(Int, Option[String])] =
    many1(del(".") /* |ellipsis*/) ~ opt(dottedName) map ({
      case dots ~ optName => (dots.size, optName)
    }, {
      case (nDots, optName) => Seq(Seq.fill(nDots)(".") ~ optName)
    })
  
  lazy val importStar: Syntax[Seq[Alias]] = op("*") map ({
    case _ => Seq(Alias("*", None))
  }, {
    case Seq(Alias("*", _)) => Seq("*")
    case _ => Seq()
  })

  lazy val asName: Syntax[Alias] =
    nameString ~ opt(kwU("as").skip ~ nameString) map ({
      case name ~ as => Alias(name, as)
    }, {
      case Alias(name, as) => Seq(name ~ as)
      case _ => Seq()
    })
  lazy val dottedAsName: Syntax[Alias] =
    dottedName ~ opt(kwU("as").skip ~ nameString) map ({
      case name ~ as => Alias(name, as)
    }, {
      case Alias(name, as) => Seq(name ~ as)
      case _ => Seq()
    })

  lazy val asNames: Syntax[Seq[Alias]] = rep1sep(asName, delU(","))
  lazy val dottedAsNames: Syntax[Seq[Alias]] = rep1sep(dottedAsName, delU(","))

  lazy val dottedName: Syntax[String] = rep1sep(nameString, delU(".")) map ({
    case strs => strs.mkString(".")
  }, {
    case str => Seq(str.split('.'))
  })

  lazy val breakStmt: Syntax[Statement] = kw("break") map ({
    case break => Break
  }, {
    case Break => Seq("break")
    case _ => Seq()
  }) 
  lazy val continueStmt: Syntax[Statement] = kw("continue") map ({
    case continue => Continue
  }, {
    case Continue => Seq("continue")
    case _ => Seq()
  })
  lazy val returnStmt: Syntax[Statement] = kwU("return").skip ~ opt(testListStarExpr) map ({
    case opt => Return(opt)
  }, {
    case Return(opt) => Seq(opt)
    case _ => Seq()
  })
  lazy val raiseStmt: Syntax[Statement] =
    kwU("raise").skip ~ opt(test ~ opt(kwU("from").skip ~ test)) map ({
      case None => Raise(None, None)
      case Some(e ~ o) => Raise(Some(e), o)
    }, {
      case Raise(None, _) => Seq(None)
      case Raise(Some(e), o) => Seq(Some(e ~ o)) 
    })

  lazy val yieldStmt: Syntax[Statement] = yieldExpr map ({
    case yieldExp => ExprStmt(yieldExp)
  }, {
    case ExprStmt(yieldExp) => Seq(yieldExp)
    case _ => Seq()
  })
  
  // What follows ':' (example after an if ...: or for ...:)
  lazy val suiteStmt: Syntax[Seq[Statement]] =
    (simpleStmt || newLine.skip ~ indent.skip ~ many1(stmt) ~ dedent.skip).map ({
      case Left(statements) => statements
      case Right(seqSeqStmts) => seqSeqStmts.reduce(_ ++ _)
    }, {
      // we always print the one statement per line version
      case statements => Seq(Right(statements.map(Seq(_))))
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
    kw("in"), notIn, isOrIsNot
  )

  lazy val notIn: Syntax[String] = kw("not") ~ kw("in") map ({
    case _ => "not in"
  }, {
    case "not in" => Seq("not" ~ "in")
    case _ => Seq()
  })

  // Special case for LL1
  lazy val isOrIsNot: Syntax[String] = kwU("is").skip ~ opt(kw("not")) map ({
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
    recursive (name | number | string | atomPredef | atomParens | atomBrackets)

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

  lazy val testList: Syntax[Expr] =
    rep1sep(test, delU(",")) map ({
      case Seq(t) => t
      case seq => Tuple(seq)
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
  val stringParser = LL1(test)
  val printer = PrettyPrinter(module)
  
  def apply(tokens: Iterator[Token]): Module = {
    if (!module.isLL1) {
      debug(module)

      throw new Error("Syntax is not LL1")
    } else {
      println("Syntax is LL1!")
    }
    
    val res = parser(tokens) match {
      case LL1.Parsed(value, rest) => value
      case LL1.UnexpectedToken(token, rest) => {
        throw new Error(f"Invalid token $token at ${token.position}")
      }
      case LL1.UnexpectedEnd(rest) => throw new Error(f"Invalid end")
    }

    println("Pretty printing: ")
    val pretty = unapply(res).get
    println(pretty)

    res
  }

  def parseExpr(tokens: Iterator[Token]): Expr = {
    stringParser(tokens) match {
      case LL1.Parsed(value, rest) => value
      case LL1.UnexpectedToken(token, rest) => throw new Error(f"Invalid token $token")
      case LL1.UnexpectedEnd(rest) => throw new Error("Invalid end")
    }
  }

  def unapply(value: Module): Option[String] = {
    printer(value).take(1).map(Lexer.unapply(_)).toList.headOption
  }
  
  def getKind(token: Token): Kind = token match {
    case _:BytesLiteral | _:IntLiteral | _:FloatLiteral | _:ImaginaryLiteral => NumberClass
    case _:StringLiteral => StringClass
    case Keyword(name) => KeywordClass(name)
    case Operator(op) => OperatorClass(op)
    case Identifier(name) => NameClass
    case Delimiter(del) => DelimiterClass(del)
    case Newline() => NewlineClass
    case Indent() => IndentClass
    case Dedent() => DedentClass
    case EOF() => EOFClass
    case _ => OtherClass
  }
}