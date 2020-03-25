package spp.parsing

import spp.utils.Pipeline
import spp.utils.Context
import spp.structure.AbstractSyntaxTree._

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
    case Name(namespace, name) => namespace match {
      case Some(s) => s ++ "." ++ name
      case None => name
    }
    case _ => "???"
  }
}