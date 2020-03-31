package spp.structure

import spp.utils.Positioned

object AbstractSyntaxTree {
  trait Tree extends Positioned
  case class Module(body: Seq[Statement]) extends Tree

  trait Statement extends Tree
  case class Assign(left: Seq[Seq[Expr]], value: Seq[Expr]) extends Statement

  
  trait Expr extends Tree
  case class BoolOp(op: String, left: Expr, right: Expr) extends Expr
  case class BinOp(op: String, left: Expr, right: Expr) extends Expr
  case class UnaryOp(op: String, expr: Expr) extends Expr
  // case class Lambda(args: Seq[Arg], body: Expr) extends Expr
  case class IfExpr(condition: Expr, ifValue: Expr, elseValue: Expr) extends Expr
  case class Dict(keys: Seq[Expr], values: Seq[Expr]) extends Expr
  case class Set(elts: Seq[Expr]) extends Expr
  // case class SeqComp
  // case class Setcomp
  // case class Dictcomp
  // case class GeneratorExpr
  // case class Await
  // case class Yield
  // case class YieldFrom
  case class Compare(left: Expr, ops: Seq[String], comparators: Seq[Expr]) extends Expr
  // case class Call
  // case class FormattedValue
  // case class JoinedStr
  // case class Constant


  case class Name(namespace: Option[String], name: String) extends Expr
  case class Attribute()
  case class Not(expr: Expr) extends Expr

  trait Arg extends Tree
}
