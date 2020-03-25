package spp.structure

import spp.utils.Positioned

object AbstractSyntaxTree {
  trait Tree extends Positioned
  case class Module(body: Seq[Statement]) extends Tree

  trait Statement extends Tree
  case class Assign(left: Seq[Seq[Expr]], value: Seq[Expr]) extends Statement
  
  trait Expr extends Tree
  case class Name(namespace: Option[String], name: String) extends Expr
  case class TernaryExpr(condition: Expr, ifValue: Expr, elseValue: Expr) extends Expr
  case class BinaryExpr(operator: String, left: Expr, right: Expr) extends Expr
}
