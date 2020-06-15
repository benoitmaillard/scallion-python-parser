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
import spp.lexer.StringDecoder.decode

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

  // separator can optionnally appear at the end and at least one repetition is required
  def rep1septr[A, B](rep: Syntax[A], sep: Syntax[Unit]): Syntax[Seq[A]] = {
    lazy val synt: Syntax[Seq[A]] = recursive(rep ~ opt(sep.skip ~ opt(synt))) map ({
      case r ~ Some(Some(f)) => r +: f
      case r ~ _ => Seq(r)
    }, {
      case Seq(r) => Seq(r ~ None)
      case r +: f => Seq(r ~ Some(Some(f)))
    })
    synt
  }

  // separator can optionnally appear at the end and 0 repetitions is accepted
  def repseptr[A, B](rep: Syntax[A], sep: Syntax[Unit]): Syntax[Seq[A]] =
    opt(rep1septr(rep, sep)) map({
      case Some(seq) => seq
      case None => Seq()
    }, {
      case Seq() => Seq(None)
      case seq => Seq(Some(seq))
    })
  
  // the boolean is true if there is a trailing separator or more than 1 element
  def rep1septrWithOpt[A, B](rep: Syntax[A], sep: Syntax[Unit]): Syntax[(Seq[A], Boolean)] = {
    lazy val synt: Syntax[(Seq[A], Boolean)] = recursive(rep ~ opt(sep.skip ~ opt(synt))) map ({
      case r ~ Some(Some(f)) => (r +: f._1, true)
      case r ~ opLast => (Seq(r), opLast.isDefined)
    }, {
      case (Seq(r), true) => Seq(r ~ Some(None))
      case (Seq(r), false) => Seq(r ~ None)
      case (r +: f, bool) => Seq(r ~ Some(Some(f, bool)))
    })
    synt
  }

  def repseptrWithOpt[A, B](rep: Syntax[A], sep: Syntax[Unit]): Syntax[(Seq[A], Boolean)] = {
    opt(rep1septrWithOpt(rep, sep)) map({
      case Some(seq) => seq
      case None => (Seq(), false)
    }, {
      case (Seq(), false) => Seq(None)
      case seq => Seq(Some(seq))
    })
  }

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

  lazy val bytes: Syntax[BytesConstant] = accept(BytesClass) ({
    case BytesLiteral(prefix, delimiter, value) => BytesConstant(decode(prefix, value).get._1)
  }, {
    case BytesConstant(value) => Seq(BytesLiteral("", "'", value))
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
  lazy val simpleStmt: Syntax[Seq[Statement]] = // TODO check why this makes the tests fail (wtf ??)
    rep1septr(smallStmt, delU(";")) ~ newLine.skip

  lazy val compoundStmt: Syntax[Statement] = ifStmt | whileStmt | forStmt | tryStmt |
    withStmt | funcDef.up[Statement] | classDef.up[Statement] | decorated | asyncStmt
  
  lazy val asyncStmt: Syntax[Statement] = kwU("async").skip ~ (funcDef.up[Statement] | withStmt | forStmt) map ({
    case f:FunctionDef => f.copy(async = true)
    case w:With => w.copy(async = true)
    case f:For => f.copy(async = true)
  }, {
    // funcDef, withStmt and forStmt only accept async=false to force unapply to go through asyncStmt
    case f@FunctionDef(_, _, _, _, _, true) => Seq(f.copy(async = false))
    case w@With(_, _, true) => Seq(w.copy(async = false))
    case f@For(_, _, _, _, true) => Seq(f.copy(async = false))
    case _ => Seq()
  })

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
      case (target, isTuple) ~ iter ~ body ~ orElse =>
        For(if (isTuple) Tuple(target) else singleExprOrTuple(target), iter, body, orElse)
    }, {
      case For(Tuple(seq), iter, body, orElse, false) => Seq((seq, true) ~ iter ~ body ~ orElse)
      case For(target, iter, body, orElse, false) => Seq((Seq(target), false) ~ iter ~ body ~ orElse)
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
      case With(items, suite, false) => Seq(items ~ suite)
      case _ => Seq()
    })

  lazy val withItem: Syntax[WithItem] =
    test ~ opt(kwU("as").skip ~ expr) map ({
      case item ~ optName => WithItem(item, optName)
    }, {
      case WithItem(item, optName) => Seq(item ~ optName)
      case _ => Seq()
    })
  
  lazy val asyncFuncdef: Syntax[FunctionDef] =
    kwU("async").skip ~ funcDef map ({
      case f => f.copy(async = true)
    }, {
      case f@FunctionDef(_, _, _, _, _, true) => Seq(f.copy(async = false))
      case _ => Seq()
    })

  lazy val funcDef: Syntax[FunctionDef] =
    kwU("def").skip ~ nameString ~ parameters ~ opt(delU("->").skip ~ test) ~
    delU(":").skip ~ suiteStmt map ({
      case name ~ params ~ optReturnType ~ body => FunctionDef(name, params, body, Seq(), optReturnType)
    }, {
      case FunctionDef(name, params, body, Seq(), optReturnType, false) =>
        Seq(name ~ params ~ optReturnType ~ body)
      case _ => Seq()
    })
  
  lazy val parameters: Syntax[Arguments] =
    delU("(").skip ~ opt(typedArgsList) ~ delU(")").skip map ({
      case Some((args, varargs, kwonly, kwargs)) => Arguments(args, varargs, kwonly, kwargs)
      case _ => Arguments(Seq(), None, Seq(), None)
    }, {
      case Arguments(Seq(), None, Seq(), None) => Seq(None)
      case Arguments(args, varargs, kwonly, kwargs) => Seq(Some((args, varargs, kwonly, kwargs)))
      case _ => Seq()
    })

  lazy val typedArgsList: Syntax[(Seq[Arg], Option[Arg], Seq[Arg], Option[Arg])] =
    tfpdefDefault ~ typedArgsList2 || typedArgsList3 map ({
      case Left(arg ~ ((args, varargs, kwonly, kwargs))) => (arg +: args, varargs, kwonly, kwargs)
      case Right((varargs, kwonly, kwargs)) => (Seq(), varargs, kwonly, kwargs)
    })
  lazy val typedArgsList2: Syntax[(Seq[Arg], Option[Arg], Seq[Arg], Option[Arg])] = recursive(
    opt(delU(",").skip ~ (tfpdefDefault ~ typedArgsList2 || opt(typedArgsList3))) map ({
      case Some(Left(arg ~ ((args, varargs, kwonly, kwargs)))) => (arg +: args, varargs, kwonly, kwargs)
      case Some(Right(Some((varargs, kwonly, kwargs)))) => (Seq(), varargs, kwonly, kwargs)
      case _ => (Seq(), None, Seq(), None)
    }))
  lazy val typedArgsList3: Syntax[(Option[Arg], Seq[Arg], Option[Arg])] =
    typedArgsList4 || opU("**").skip ~ tfpdef ~ opt(delU(",")) map ({
      case Left(args) => args
      case Right(arg ~ _) => (None, Seq(), Some(arg))
    })
    
  lazy val typedArgsList4: Syntax[(Option[Arg], Seq[Arg], Option[Arg])] =
    opU("*").skip ~ opt(tfpdef) ~ typedArgsList5 map ({
      case optArg ~ ((kwonly, kwargs)) => (optArg, kwonly, kwargs)
    })
  lazy val typedArgsList5: Syntax[(Seq[Arg], Option[Arg])] = recursive(
    opt(delU(",").skip ~ (tfpdefDefault ~ typedArgsList5 || opt(opU("**").skip ~ tfpdef ~ opt(delU(","))))) map ({
      case Some(Left(arg ~ ((kwonly, kwargs)))) => (arg +: kwonly, kwargs)
      case Some(Right(Some(kwargs ~ _))) => (Seq(), Some(kwargs))
      case _ => (Seq(), None)
    })
  )

  lazy val tfpdefDefault: Syntax[Arg] =
    tfpdef ~ opt(delU("=").skip ~ test) map ({
      case arg ~ optDft => Arg(arg.arg, arg.annotation, optDft)
    }, {
      case Arg(name, optAnn, optDft) => Seq(Arg(name, optAnn, None) ~ optDft)
      case _ => Seq()
    })

  lazy val tfpdef: Syntax[Arg] =
    nameString ~ opt(delU(":").skip ~ test) map ({
      case name ~ optAnn => Arg(name, optAnn, None)
    }, {
      case Arg(name, optAnn, None) => Seq(name ~ optAnn)
      case _ => Seq()
    })

  lazy val varArgsList: Syntax[(Seq[Arg], Option[Arg], Seq[Arg], Option[Arg])] =
    vfdefDefault ~ varArgsList2 || varArgsList3 map ({
      case Left(arg ~ ((args, varargs, kwonly, kwargs))) => (arg +: args, varargs, kwonly, kwargs)
      case Right((varargs, kwonly, kwargs)) => (Seq(), varargs, kwonly, kwargs)
    })
  lazy val varArgsList2: Syntax[(Seq[Arg], Option[Arg], Seq[Arg], Option[Arg])] = recursive(
    opt(delU(",").skip ~ (vfdefDefault ~ varArgsList2 || opt(varArgsList3))) map ({
      case Some(Left(arg ~ ((args, varargs, kwonly, kwargs)))) => (arg +: args, varargs, kwonly, kwargs)
      case Some(Right(Some((varargs, kwonly, kwargs)))) => (Seq(), varargs, kwonly, kwargs)
      case _ => (Seq(), None, Seq(), None)
    }))
  lazy val varArgsList3: Syntax[(Option[Arg], Seq[Arg], Option[Arg])] =
    varArgsList4 || opU("**").skip ~ vfdef ~ opt(delU(",")) map ({
      case Left(args) => args
      case Right(arg ~ _) => (None, Seq(), Some(arg))
    })
    
  lazy val varArgsList4: Syntax[(Option[Arg], Seq[Arg], Option[Arg])] =
    opU("*").skip ~ opt(vfdef) ~ varArgsList5 map ({
      case optArg ~ ((kwonly, kwargs)) => (optArg, kwonly, kwargs)
    })
  lazy val varArgsList5: Syntax[(Seq[Arg], Option[Arg])] = recursive(
    opt(delU(",").skip ~ (vfdefDefault ~ varArgsList5 || opt(opU("**").skip ~ vfdef ~ opt(delU(","))))) map ({
      case Some(Left(arg ~ ((kwonly, kwargs)))) => (arg +: kwonly, kwargs)
      case Some(Right(Some(kwargs ~ _))) => (Seq(), Some(kwargs))
      case _ => (Seq(), None)
    })
  )

  lazy val vfdefDefault: Syntax[Arg] =
    vfdef ~ opt(delU("=").skip ~ test) map ({
      case arg ~ optDft => Arg(arg.arg, arg.annotation, optDft)
    }, {
      case Arg(name, optAnn, optDft) => Seq(Arg(name, optAnn, None) ~ optDft)
      case _ => Seq()
    })
  
  lazy val vfdef: Syntax[Arg] = nameString map ({
    case n => Arg(n, None, None)
  }, {
    case Arg(name, None, None) => Seq(name)
    case _ => Seq()
  })

  lazy val classDef: Syntax[ClassDef] =
    kwU("class").skip ~ nameString ~ opt(delU("(").skip ~ argList ~ delU(")").skip) ~
    delU(":").skip ~ suiteStmt map ({
      case name ~ Some(bases) ~ body => ClassDef(name, bases, body, Seq())
      case name ~ None ~ body => ClassDef(name, Seq(), body, Seq())
    }, {
      case ClassDef(name, Seq(), body, Seq()) => Seq(name ~ None ~ body)
      case ClassDef(name, bases, body, Seq()) => Seq(name ~ Some(bases) ~ body)
      case _ => Seq()
    })

  lazy val decorator: Syntax[Expr] =
    opU("@").skip ~ rep1sep(nameString, delU(".")) ~ opt(delU("(").skip ~ argList ~ delU(")").skip) ~ newLine.skip map ({
      case names ~ optArgs => {
        val target = names.tail.foldLeft[Expr](Name(names.head)){
          case (acc, cur) => Attribute(acc, cur)
        }
        optArgs.map(Call(target, _)).getOrElse(target)
      }
    })
  
  lazy val decorators = many1(decorator)

  lazy val decorated: Syntax[Statement] = decorators ~ (classDef || (funcDef | asyncFuncdef)) map ({
    case ds ~ Left(clazz) => clazz.copy(decorators = ds)
    case ds ~ Right(fun) => fun.copy(decorators = ds)
  })

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
      case e ~ Some(Right((ann, opt))) => AnnAssign(e, ann, opt, e match {case Name(name) => true case _ => false})
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
    case (exps, _) => Delete(exps)
  }, {
    case Delete(exps) => Seq((exps, false))
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
    many1(del(".") | op("...")) ~ opt(dottedName) map ({
      case dots ~ optName => (dots.map(_.size).sum, optName)
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

  lazy val asNames: Syntax[Seq[Alias]] = rep1septr(asName, delU(","))
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

  lazy val test: Syntax[Expr] = recursive(test2 | lambdef)
  lazy val test2: Syntax[Expr] = 
    orTest ~ opt(kwU("if").skip ~ orTest ~ kwU("else").skip ~ test) map ({
      case vTrue ~ Some(condition ~ vFalse) => IfExpr(condition, vTrue, vFalse)
      case v ~ None => v
    }, {
      case IfExpr(condition, vTrue, vFalse) => Seq(vTrue ~ Some(condition ~ vFalse))
      case e => Seq(e ~ None)
    })

  lazy val testNoCond: Syntax[Expr] = orTest | recursive(lambdefNoCond)

  lazy val lambdef: Syntax[Expr] =
    kwU("lambda").skip ~ opt(varArgsList) ~ delU(":").skip ~ test map ({
      case Some((args, varargs, kwonly, kwargs)) ~ e => Lambda(Arguments(args, varargs, kwonly, kwargs), e)
      case None ~ e => Lambda(Arguments(Seq(), None, Seq(), None), e)
    })

  // TODO unapply
  lazy val lambdefNoCond: Syntax[Expr] =
    kwU("lambda").skip ~ opt(varArgsList) ~ delU(":").skip ~ testNoCond map ({
      case Some((args, varargs, kwonly, kwargs)) ~ e => Lambda(Arguments(args, varargs, kwonly, kwargs), e)
      case None ~ e => Lambda(Arguments(Seq(), None, Seq(), None), e)
    })

  lazy val orTest: Syntax[Expr] = rep1sep(andTest, kwU("or")) map ({
    case Seq(value) => value
    case values => BoolOp("or", values)
  }, {
    case BoolOp("or", values) => Seq(values)
    case value => Seq(Seq(value))
  })


  lazy val andTest: Syntax[Expr] = rep1sep(notTest, kwU("and")) map ({
    case Seq(value) => value
    case values => BoolOp("and", values)
  }, {
    case BoolOp("and", values) => Seq(values)
    case value => Seq(Seq(value))
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

  lazy val expr: Syntax[Expr] = operators(factor)(
    op("*") | op("@") | op("/") | op("%") | op("//") is LeftAssociative,
    op("+") | op("-") is LeftAssociative,
    op("<<") | op(">>") is LeftAssociative,
    op("&") is LeftAssociative,
    op("^") is LeftAssociative,
    op("|") is LeftAssociative,
  )({
    case (l, op, r) => BinOp(op, l, r)
  }, {
    case BinOp(op, l, r) => (l, op, r)
  })

  lazy val factor: Syntax[Expr] = recursive((op("+") | op("-") | op("~")) ~ factor || power) map ({
    case Left(o ~ e) => UnaryOp(o, e)
    case Right(e) => e
  }, {
    case UnaryOp(o, e) => Seq(Left(o ~ e))
    case e => Seq(Right(e))
  })

  // operators can not be used because of asymmetry (atomExpr - factor)
  lazy val power: Syntax[Expr] = atomExprAwait ~ opt(opU("**").skip ~ factor) map ({
    case left ~ None => left
    case left ~ Some(right) => BinOp("**", left, right)
  }, {
    case BinOp("**", left, right) => Seq(left ~ Some(right))
    case left => Seq(left ~ None)
  })

  lazy val atomExprAwait: Syntax[Expr] = opt(kw("await")) ~ atomExpr map({
    case awt ~ exp => awt.map(_ => Await(exp)).getOrElse(exp)
  }, {
    case Await(e) => Seq(Some("await") ~ e)
    case e => Seq(None ~ e)
  })

  lazy val atomExpr: Syntax[Expr] = atom ~ (many(trailer)) map ({
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

  lazy val atom: Syntax[Expr] =
    recursive (name | number | atomString | atomBytes | atomPredef | atomParens | atomBrackets | atomBraces)

  lazy val atomString: Syntax[Expr] = many1(string) map ({
    case Seq(string) => string // this case must be handled separately because a f-string with no format field should stay a JoinedStr 
    case strings => strings.map{
      case c:StringConstant => Seq(c)
      case JoinedStr(values) => values
    }.flatten.foldLeft(Seq.empty[Expr]){
      case (init :+ StringConstant(v1), StringConstant(v2)) => init :+ StringConstant(v1 + v2)
      case (acc, e) => acc :+ e
    } match {
      case head :: Nil => head
      case parts => JoinedStr(parts)
    }
  })

  lazy val atomBytes: Syntax[Expr] = many1(bytes) map ({
    case seq => BytesConstant(seq.map(_.value).mkString)
  })

  // parenthesized expression
  lazy val atomParens: Syntax[Expr] =
    delU("(").skip ~ opt(yieldExpr | atomParensContent) ~ delU(")").skip map ({
      case None => Tuple(Seq.empty) // empty parenthesis is the 0-tuple
      case Some(e) => e
    }, {
      case Tuple(Seq()) => Seq(None)
      case e => Seq(Some(e))
    })
  lazy val atomParensContent: Syntax[Expr] = testListComp map ({
    case Left((e, comps)) => GeneratorExp(e, comps)
    case Right((Seq(e), isTuple)) => if (isTuple) Tuple(Seq(e)) else e // (x,) is a 1-tuple
    case Right((exprs, _)) => Tuple(exprs)
  }, {
    case GeneratorExp(e, comps) => Seq(Left((e, comps)))
    case Tuple(exprs) => Seq(Right(exprs, true))
    case e => Seq(Right(Seq(e), false))
  })

  // bracketized expression, either a list literal or a list comprehension
  lazy val atomBrackets: Syntax[Expr] =
    delU("[").skip ~ opt(atomBracketsContent) ~ delU("]").skip map ({
      case None => PythonList(Seq.empty)
      case Some(e) => e
    }, {
      case PythonList(Seq()) => Seq(None)
      case e => Seq(Some(e))
    })
  lazy val atomBracketsContent: Syntax[Expr] = testListComp map ({
    case Left((e, comps)) => ListComp(e, comps)
    case Right((exprs, _)) => PythonList(exprs)
  }, {
    case ListComp(e, comps) => Seq(Left((e, comps)))
    case PythonList(exprs) => Seq(Right(exprs, false))
    case _ => Seq()
  })

  // expression in braces, either a dict literal or a set comprehension
  lazy val atomBraces: Syntax[Expr] =
    delU("{").skip ~ opt(dictOrSetMaker) ~ delU("}").skip map ({
      case None => Dict(Seq())
      case Some(e) => e
    }, {
      case Dict(Seq()) => Seq(None)
      case e => Seq(Some(e))
    })

  lazy val dictOrSetMaker: Syntax[Expr] =
    dictOrSetMaker1.up[Expr] | dictOrSetMaker2.up[Expr] | dictOrSetMaker3
  lazy val dictOrSetMaker1: Syntax[Dict] = opU("**").skip ~ expr ~ keyvalList map ({
    case e ~ next => Dict(KeyVal(None, e) +: next)
  })
  lazy val dictOrSetMaker2: Syntax[Set] = starExpr ~ seteltsList map ({
    case e ~ next => Set(e +: next)
  })
  lazy val dictOrSetMaker3: Syntax[Expr] =
    test ~ (delU(":").skip ~ test ~ (compFor || keyvalList) || (compFor || seteltsList)) map ({
      case e1 ~ Left(e2 ~ Left(comp)) => DictComp(KeyVal(Some(e1), e2), comp)
      case e1 ~ Left(e2 ~ Right(keyvals)) => Dict(KeyVal(Some(e1), e2) +: keyvals)
      case e1 ~ Right(Left(comp)) => SetComp(e1, comp)
      case e1 ~ Right(Right(vals)) => Set(e1 +: vals)
    })

  lazy val keyvalList: Syntax[Seq[KeyVal]] = opt(delU(",").skip ~ repseptr(keyval, delU(","))) map ({
    case opt => opt.getOrElse(Seq())
  }) /* [','] */
  lazy val keyval: Syntax[KeyVal] = test ~ delU(":").skip ~ test || opU("**").skip ~ expr map ({
    case Left(key ~ value) => KeyVal(Some(key), value)
    case Right(e) => KeyVal(None, e)
  })

  lazy val seteltsList: Syntax[Seq[Expr]] = opt(delU(",").skip ~ repseptr(test | starExpr, delU(","))) map ({
    case opt => opt.getOrElse(Seq())
  })
  
  // None, True, False
  lazy val atomPredef: Syntax[Expr] = (kw("True") | kw("False") | kw("None") | op("...")) map ({
    case "True" => BooleanConstant(true)
    case "False" => BooleanConstant(false)
    case "None" => NoneValue
    case "..." => Ellipsis
  }, {
    case BooleanConstant(bool) => Seq(if (bool) "True" else "False")
    case NoneValue => Seq("None")
    case Ellipsis => Seq("...")
    case _ => Seq()
  })
  
  // something immediately after an atomic expression
  lazy val trailer: Syntax[Either[String, Either[Seq[CallArg], Slice]]] =
    trailerName || (trailerCall || trailerSubscript)

  lazy val trailerCall: Syntax[Seq[CallArg]] =
    delU("(").skip ~ argList ~ delU(")").skip
  lazy val argList: Syntax[Seq[CallArg]] = repseptr(argument, delU(","))

  lazy val trailerSubscript: Syntax[Slice] =
    delU("[").skip ~ subscriptList ~ delU("]").skip

  lazy val subscriptList: Syntax[Slice] = rep1septrWithOpt(subscript, delU(",")) map ({
      case (Seq(sl), false) => sl
      case (slices, true) => 
        if (slices.exists{case _:DefaultSlice => true case _ => false}) ExtSlice(slices)
        else Index(Tuple(slices.collect{case i:Index => i}.map(_.value)))
    }, {
      case ExtSlice(slices) => Seq((slices, true))
      case Index(Tuple(exps)) => Seq((exps.map(Index(_)), true))
      case s@(_:DefaultSlice | _:Index) => Seq((Seq(s), false))
    })

  lazy val trailerName: Syntax[String] = delU(".").skip ~ nameString

  lazy val argument: Syntax[CallArg] = recursive(argumentStartingWithTest | argumentStar | argumentDoubleStar)
  lazy val argumentStartingWithTest: Syntax[CallArg] =
    test ~ opt(opU(":=").skip ~ test || delU("=").skip ~ test || compFor) map ({
      case e1 ~ o => o match {
        case None => PosArg(e1)
        case Some(Left(Left(e2))) => PosArg(NamedExpr(e1, e2))
        case Some(Left(Right(e2))) => KeywordArg(Some(e1), e2)
        case Some(Right(comp)) => PosArg(GeneratorExp(e1, comp))
      }
    }, {
      case KeywordArg(Some(e1), e2) => Seq(e1 ~ Some(Left(Right(e2))))
      case PosArg(NamedExpr(e1, e2)) => Seq(e1 ~ Some(Left(Left(e2))))
      case PosArg(GeneratorExp(e1, comp)) => Seq(e1 ~ Some(Right(comp)))
      case PosArg(e1) => Seq(e1 ~ None)
      case _ => Seq()
    })

  lazy val argumentStar: Syntax[CallArg] = starExpr map ({
    case e => PosArg(e)
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
  // the boolean value indicates wether or not the seq of expr should be considered as a tuple
  // NOTE : grammar specification seems to forget to mention the case with 1-tuple (',' before namedOrStarListComp)
  lazy val testListComp: Syntax[Either[(Expr, Seq[Comprehension]), (Seq[Expr], Boolean)]] =
    (namedExprTest | starExpr) ~ (compFor || opt(delU(",").skip ~ namedOrStarListComp)) map ({
      case e ~ Left(comps) => Left(e, comps)
      case e ~ Right(Some(exprs)) => Right(e +: exprs, true) // >= 2 expressions OR 1 expression with trailing comma
      case e ~ Right(None) => Right(Seq(e), false) // 1 expression without trailing comma
    }, {
      case Left((e, comps)) => Seq(e ~ Left(comps))
      case Right((e +: exprs, true)) => Seq(e ~ Right(Some(exprs)))
      case Right((Seq(e), false)) => Seq(e ~ Right(None))
      case _ => Seq()
    })
  
  lazy val compFor: Syntax[Seq[Comprehension]] = /*[async]*/
    opt(kwU("async")) ~ kwU("for").skip ~ exprList ~ kwU("in").skip ~ orTest ~ opt(compIter) map ({
      case async ~ ((forExps, isTuple)) ~ inExp ~ optFollow => optFollow match {
        case None => Seq(Comprehension(if (isTuple) Tuple(forExps) else singleExprOrTuple(forExps), inExp, Seq.empty, async.isDefined))
        case Some((ifs, comps)) => {
          val headComp = Comprehension(singleExprOrTuple(forExps), inExp, ifs, async.isDefined)
          headComp +: comps
        }
      }
    }/*, {
      case Seq(Comprehension(target, iter, Seq())) => Seq((Seq(target), false) ~ iter ~ None)
      case Comprehension(target, iter, ifs) :: tail => Seq((Seq(target), false) ~ iter ~ Some(ifs, tail))
      case _ => Seq()
    }*/)
  
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
  
  lazy val exprList: Syntax[(Seq[Expr], Boolean)] = rep1septrWithOpt((expr | starExpr), delU(","))

  lazy val namedOrStarListComp: Syntax[Seq[Expr]] =
    repseptr(namedExprTest | starExpr, delU(","))

  // testListStarExpr and testList share the same map
  val testListMap: ((Seq[Expr], Boolean)) => Expr = {
    case (Seq(tail), false) => tail
    case (seq, isTuple) => Tuple(seq)
  }
  val testListMapReverse: Expr => Seq[(Seq[Expr], Boolean)] = {
    case Tuple(seq) => Seq((seq, true))
    case e => Seq((Seq(e), false))
  }

  lazy val testListStarExpr: Syntax[Expr] =
    rep1septrWithOpt(test | starExpr, delU(",")) map (testListMap, testListMapReverse)
  
  lazy val testList: Syntax[Expr] =
    rep1septrWithOpt(test, delU(",")) map (testListMap, testListMapReverse)
  
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
    }
    
    val res = parser(tokens) match {
      case LL1.Parsed(value, rest) => value
      case LL1.UnexpectedToken(token, rest) => {
        throw new Error(f"Invalid token $token at ${token.position}")
      }
      case LL1.UnexpectedEnd(rest) => throw new Error(f"Invalid end")
    }

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
    case _:IntLiteral | _:FloatLiteral | _:ImaginaryLiteral => NumberClass
    case _:StringLiteral => StringClass
    case _:BytesLiteral => BytesClass
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