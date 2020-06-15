package spp.parsing

import spp.structure.AbstractSyntaxTree._
import scala.util.{Try, Success, Failure}

import scala.language.implicitConversions

object TreeValidation {
    def validate(tree: Module): Try[Unit] = validateNode(tree)

    private implicit def validateSeq(nodes: Seq[Tree]): Try[Unit] =
      reduceAll(nodes.map(validateNode(_)))
    private implicit def validateOption(opt: Option[Tree]): Try[Unit] =
      opt.map(validateNode(_)).getOrElse(Success())
    
    // TODO does not belong here
    case class SyntaxError(msg: String, position: Int = 0) extends Error(msg)
    
    def reduce(args: Try[Unit]*): Try[Unit] = reduceAll(args)
    def reduceAll(args: Seq[Try[Unit]]): Try[Unit] = Try(args.map(_.get))

    private implicit def validateNode(node: Tree): Try[Unit] = node match {
        case Module(body) => validateSeq(body)
        case FunctionDef(name, args, body, decorators, returns, async) =>
          reduce(args, body, decorators, returns)
        case ClassDef(name, bases, body, decorators) =>
          reduce(validateCallArgs(bases), body, decorators)
        case Return(value) =>
          value
        case Delete(targets) =>
          reduceAll(targets.map(validateAssignable(_)))
        case Assign(targets, value) =>
          reduce(
            reduceAll(targets.map(validateAssignable(_))),
            value
          )
        case AugAssign(target, op, value) =>
          reduce(
            validateAssignable(target),
            value
          )
        // there is surprisingly no limitation on the annotation
        case AnnAssign(target, annotation, value, simple) =>
          reduce(
            validateAssignable(target),
            value
          )
        case For(target, iter, body, orelse, async) =>
          reduce(validateAssignable(target), iter, body, orelse)
        case While(test, body, orelse) =>
          reduce(test, body, orelse)
        case If(test, body, orelse) =>
          reduce(test, body, orelse)
        case With(items, body, async) =>
          reduce(items, body)
        case Raise(exc, cause) =>
          reduce(exc, cause)
        case spp.structure.AbstractSyntaxTree.Try(body, handlers, orelse, finalbody) =>
          reduce(body, handlers, orelse, finalbody)
        case Assert(test, msg) =>
          reduce(test, msg)
        case _:Import | _:ImportFrom | _:Global | _:Nonlocal | Pass | Break | Continue => Success()
        case ExprStmt(value) => value

        case BoolOp(op, values) => values
        case NamedExpr(target, value) => reduce(target, value)
        case BinOp(op, left, right) => reduce(left, right)
        case UnaryOp(op, expr) => expr
        case Lambda(args, body) => reduce(args, body)
        case IfExpr(condition, ifValue, elseValue) => reduce(condition, ifValue, elseValue)
        case Dict(elts) => reduce(elts)
        case Set(elts) => reduce(elts)
        case ListComp(elt, generators) => reduce(elt, generators)
        case SetComp(elt, generators) => reduce(elt, generators)
        case DictComp(elt, generators) => reduce(elt, generators)
        case GeneratorExp(elt, generators) => reduce(elt, generators)
        case Await(value) => value
        case Yield(value) => value
        case YieldFrom(value) => value
        case Compare(left, ops, comparators) => reduce(left, comparators)
        case Call(func, args) => reduce(func, args, validateCallArgs(args))
        case FormattedValue(value, conversion, format) => reduce(value, format) // TODO check conversion ?
        case JoinedStr(values) => values
        case _:Constant => Success()
        case Attribute(value, attr) => value
        case Subscript(value, slice) => reduce(value, slice)
        case Starred(value) => value
        case Name(name) => Success()
        case PythonList(elts) => elts
        case Tuple(elts) => elts
        case arguments@Arguments(args, vararg, kwonly, kwarg) =>
          reduce(args, vararg, kwonly, kwarg, validateArgumentNames(arguments))
        case Arg(arg, annotation, default) => reduce(annotation, default)
        case KeyVal(key, value) => reduce(key, value)
        case PosArg(value) => value
        case KeywordArg(arg, value) => println(arg);arg.map(validateName(_)).getOrElse(Success())
        case Comprehension(target, iter, ifs, async) => reduce(target, validateAssignable(iter), ifs)
        case Alias(name, asname) => Success()
        case ExceptionHandler(tpe, name, body) => reduce(tpe, body)
        case WithItem(contextExpr, optionalVars) => reduce(contextExpr, optionalVars)
        case DefaultSlice(lower, upper, step) => reduce(lower, upper, step)
        case ExtSlice(dims) => reduce(dims)
        case Index(value) => value
        case _ => Success()

    }

    def validateArgumentNames(arguments: Arguments): Try[Unit] = {
      val names = List(
        arguments.args.map(_.arg), arguments.vararg.map(Seq(_)).getOrElse(Seq()),
        arguments.kwonly.map(_.arg), arguments.kwarg.map(Seq(_)).getOrElse(Seq())
      ).flatten
      
      if (names.toSet.size != names.size)
        Failure(SyntaxError("duplicate argument in function definition"))
      else {
        Success()
      }
    }

    def validateName(expr: Expr): Try[Unit] = expr match {
      case Name(name) => Success()
      case _ => Failure(SyntaxError("argument must be a name"))
    }

    def validateCallArgs(args: Seq[CallArg]): Try[Unit] = Success()
      //args.foldLeft(0)((level, arg) => if (arg >= level) arg)

    /*def argRank(arg: CallArg): Int = arg match {
      case PosArg(Starred(_)) => 1
      case PosArg(_) => 0
      case KeywordArg(Some(_), _) => 2
      case KeywordArg(None, _) => 3
    }*/

    def validateAssignable(expr: Expr): Try[Unit] = expr match {
      case _:Attribute | _:Subscript | _:Name => Success()
      case Tuple(elts) => reduceAll(elts.map(validateAssignable(_))) // unpacking is recursive
      case _ => Failure(SyntaxError("cannot assign to left hand-side"))
    }
}