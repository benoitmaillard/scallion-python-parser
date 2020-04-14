package spp.structure

import spp.utils.Positioned

object AbstractSyntaxTree {
  trait Tree extends Positioned
  case class Module(body: Seq[Statement]) extends Tree

  trait Statement extends Tree
  case class FunctionDef(name: String, args: Arguments, body: Seq[Statement], decorators: Seq[Expr], returns: Option[Expr]) extends Statement
  case class AsyncFunctionDef() extends Statement
  case class ClassDef() extends Statement
  case class Return(value: Option[Expr]) extends Statement
  case class Delete(targets: Seq[Expr]) extends Statement
  case class Assign(targets: Seq[Expr], value: Expr) extends Statement
  case class AugAssign(target: Expr, op: String, value: Expr) extends Statement
  case class AnnAssign(target: Expr, annotation: Expr, value: Option[Expr], simple: Boolean = false) extends Statement
  case class For(target: Expr, iter: Expr, body: Seq[Statement], orelse: Seq[Statement]) extends Statement
  case class AsyncFor(target: Expr, iter: Expr, body: Seq[Statement], orelse: Seq[Statement]) extends Statement
  case class While(test: Expr, body: Seq[Statement], orelse: Seq[Statement]) extends Statement
  case class If(test: Expr, body: Seq[Statement], orelse: Seq[Statement]) extends Statement
  case class With(items: Seq[WithItem], body: Seq[Statement]) extends Statement
  case class AsyncWith(items: Seq[WithItem], body: Seq[Statement]) extends Statement
  case class Raise(exc: Option[Expr], cause: Option[Expr]) extends Statement
  case class Try(body: Seq[Statement], handlers: Seq[ExceptionHandler], orelse: Seq[Statement], finalbody: Seq[Statement]) extends Statement
  case class Assert(test: Expr, msg: Option[Expr]) extends Statement
  case class Import(names: Seq[Alias]) extends Statement
  case class ImportFrom(module: Option[String], names: Seq[Alias], level: Option[Int]) extends Statement
  case class Global(names: Seq[String]) extends Statement
  case class Nonlocal(names: Seq[String]) extends Statement
  case object Pass extends Statement
  case object Break extends Statement
  case object Continue extends Statement
  case class ExprStmt(value: Expr) extends Statement

  trait Expr extends Tree
  case class BoolOp(op: String, left: Expr, right: Expr) extends Expr
  case class NamedExpr(target: Expr, value: Expr) extends Expr
  case class BinOp(op: String, left: Expr, right: Expr) extends Expr
  case class UnaryOp(op: String, expr: Expr) extends Expr
  case class IfExpr(condition: Expr, ifValue: Expr, elseValue: Expr) extends Expr
  case class Dict(keys: Seq[Expr], values: Seq[Expr]) extends Expr
  case class Set(elts: Seq[Expr]) extends Expr
  case class ListComp(elt: Expr, generators: Seq[Comprehension]) extends Expr
  case class SetComp(elt: Expr, generators: Seq[Comprehension]) extends Expr
  case class Dictcomp(key: Expr, value: Expr, generators: Seq[Comprehension]) extends Expr
  case class GeneratorExp(elt: Expr, generators: Seq[Comprehension]) extends Expr
  case class Await(value: Expr) extends Expr
  case class Yield(value: Option[Expr]) extends Expr
  case class YieldFrom(value: Expr) extends Expr
  case class Compare(left: Expr, ops: Seq[String], comparators: Seq[Expr]) extends Expr
  // NOTE: documentation has two separate lists for args, keyword args
  case class Call(func: Expr, args: Seq[CallArg]) extends Expr
  case class FormattedValue() extends Expr
  case class JoinedStr() extends Expr

  trait Constant extends Expr
  case class IntConstant(value: BigInt) extends Constant
  case class FloatConstant(value: Float) extends Constant
  case class ImaginaryConstant(value: Float) extends Constant
  case class StringConstant(prefix: Option[String], value: String) extends Constant

  case class Attribute(value: Expr, attr: String) extends Expr
  case class Subscript(value: Expr, slice: Slice) extends Expr
  case class Starred(value: Expr) extends Expr
  case class Name(name: String) extends Expr
  case class List(elts: Seq[Expr]) extends Expr
  case class Tuple(elts: Seq[Expr]) extends Expr

  case class Arg(arg: String, annotation: Option[Expr], default: Option[Expr]) extends Tree

  trait CallArg extends Tree
  case class PosArg(value: Expr) extends CallArg
  // arg is None when an argument of the type **kwargs is passed
  // arg is an expression but we have to check that it is actually name later
  case class KeywordArg(arg: Option[Expr], value: Expr) extends CallArg

  case class Comprehension(target: Expr, iter: Expr, ifs: Seq[Expr] /*isAsync*/) extends Tree

  case class Alias(name: String, asname: Option[String]) extends Tree

  case class ExceptionHandler(tpe: Option[Expr], name: Option[String], body: Seq[Statement])

  case class WithItem(contextExpr: Expr, optionalVars: Option[Expr])

  // kwonly are the args placed after vararg
  case class Arguments(args: Seq[Arg], vararg: Option[Arg], kwonly: Seq[Arg], kwarg: Option[Arg]) extends Tree

  trait Slice extends Tree
  case class DefaultSlice(lower: Option[Expr], upper: Option[Expr], step: Option[Expr]) extends Slice
  // TODO should make the difference between slice and compositeslice
  case class ExtSlice(dims: Seq[Slice]) extends Slice
  case class Index(value: Expr) extends Slice
}
