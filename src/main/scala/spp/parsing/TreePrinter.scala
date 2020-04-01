package spp.parsing

import spp.utils.Pipeline
import spp.utils.Context
import spp.structure.AbstractSyntaxTree._

import scala.language.implicitConversions

object TreePrinter extends Pipeline[Module, Unit] {

  override def run(ctx: Context)(m: Module): Unit = {
    println(printTree(m))
  }

  def printTree(t: Tree): String = t match {
    case Module(body) => body.map(printTree(_)).reduce(_ ++ "\n" ++ _)
    case Assign(seqSeqExpr, seqExpr) => {
      val concat = seqSeqExpr :+ seqExpr
      concat.map(exprSeq => exprSeq.map(printTree(_)).reduce(_ ++ ", " ++ _))
        .reduce(_ ++ " = " ++ _)
    }
    case Name(name) => name
    case IfExpr(condition, ifValue, elseValue) =>
      f"(${printTree(ifValue)} if ${printTree(condition)} else ${printTree(elseValue)})"
    case BinOp(op, left, right) =>
      f"(${printTree(left)} $op ${printTree(right)})"
    case BoolOp(op, left, right) => 
      f"(${printTree(left)} $op ${printTree(right)})"
    case UnaryOp(op, e) =>
      f"($op ${printTree(e)})"
    case Compare(left, ops, comparators) =>
      val right = (ops zip comparators).map{ case (op, comp) => op + " " + printTree(comp) }.reduce((x, y) => x + " " + y)
      f"(${printTree(left)} ${right} )"
    case _ => "???"
  }
}