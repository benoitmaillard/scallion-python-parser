package spp.structure

import spp.utils.Positioned

object AbstractSyntaxTree {
  trait Tree extends Positioned
  case class Module(body: Seq[Statement]) extends Tree

  trait Statement extends Tree
  case class FunctionDef() extends Statement
  case class AsyncFunctionDef() extends Statement
  case class ClassDef() extends Statement
  case class Return() extends Statement
  case class Delete() extends Statement
  case class Assign(left: Seq[Seq[Expr]], value: Seq[Expr]) extends Statement
  case class AugAssign() extends Statement
  case class AnnAssign() extends Statement
  case class For() extends Statement
  case class AsyncFor() extends Statement
  case class While() extends Statement
  case class If() extends Statement
  case class With() extends Statement
  case class AsyncWith() extends Statement
  case class Raise() extends Statement
  case class Try() extends Statement
  case class Assert() extends Statement
  case class Import() extends Statement
  case class ImportFrom() extends Statement
  case class Global() extends Statement
  case class Nonlocal() extends Statement
  case object Pass extends Statement
  case object Break extends Statement
  case object Continue extends Statement

  trait Expr extends Tree
  case class BoolOp(op: String, left: Expr, right: Expr) extends Expr
  case class NamedExpr(target: Expr, value: Expr) extends Expr
  case class BinOp(op: String, left: Expr, right: Expr) extends Expr
  case class UnaryOp(op: String, expr: Expr) extends Expr
  case class IfExpr(condition: Expr, ifValue: Expr, elseValue: Expr) extends Expr
  case class Dict(keys: Seq[Expr], values: Seq[Expr]) extends Expr
  case class Set(elts: Seq[Expr]) extends Expr
  case class ListComp() extends Expr
  case class SeqComp() extends Expr
  case class Setcomp() extends Expr
  case class Dictcomp() extends Expr
  case class GeneratorExpr() extends Expr
  case class Await() extends Expr
  case class Yield() extends Expr
  case class YieldFrom() extends Expr
  case class Compare(left: Expr, ops: Seq[String], comparators: Seq[Expr]) extends Expr
  case class Call() extends Expr
  case class FormattedValue() extends Expr
  case class JoinedStr() extends Expr
  case class Constant() extends Expr
  case class Attribute(value: Expr, attr: String) extends Expr
  case class Subscript(value: Expr, slice: Slice) extends Expr
  case class Starred(value: Expr) extends Expr
  case class Name(name: String) extends Expr
  case class List(elts: Expr) extends Expr
  case class Tuple(elts: Expr) extends Expr

  case class Arg(arg: String, annotation: Option[Expr], typeComment: Option[String]) extends Tree

  trait Slice extends Tree
  case class DefaultSlice(lower: Option[Expr], upper: Option[Expr], step: Option[Expr]) extends Slice
  case class ExtSlice() extends Slice
  case class Index(value: Expr) extends Slice
}
