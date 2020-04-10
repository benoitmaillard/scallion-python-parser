package spp.parsing

import spp.utils.Pipeline
import spp.utils.Context
import spp.structure.AbstractSyntaxTree._

import scala.language.implicitConversions

object TreePrinter {

  def apply(ctx: Context, m: Module): Unit = {
    println(printTree(m))
  }

  def printSeq(seq: Seq[Tree]) = seq.map(printTree(_)).reduce(_ ++ " " ++ _)

  def printTree(t: Tree): String = t match {
    case Module(body) => body.map(printTree(_)).reduce(_ ++ "\n" ++ _)
    case Assign(seqSeqExpr, seqExpr) => {
      val concat = seqSeqExpr :+ seqExpr
      concat.map(printTree(_))
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
      f"(${printTree(left)} ${right})"
    case IntConstant(value) => value.toString()
    case FloatConstant(value) => value.toString()
    case ImaginaryConstant(value) => f"${value}j"
    case Call(func, args) => args match {
      case Nil => f"${printTree(func)}()"
      case _ => f"${printTree(func)}(${args.map(printTree(_)).reduce(_ + ", " + _)})"
    }
    case Subscript(value, slice) => f"${printTree(value)}[${printTree(slice)}]"
    case Attribute(value, attr) => f"${printTree(value)}.$attr"
    case PosArg(value) => f"${printTree(value)}"
    case KeywordArg(arg, value) => arg match {
      case Some(name) => f"${printTree(name)}=${printTree(value)}"
      case None => f"**${printTree(value)}"
    }

    case ExtSlice(dims) => dims.map(printTree(_)).reduce(_ + ", " + _)
    case Index(value) => printTree(value)
    case DefaultSlice(lower, upper, step) =>
      f"${lower.map(printTree(_)).getOrElse("")}:${upper.map(printTree(_)).getOrElse("")}:${step.map(printTree(_)).getOrElse("")}"
    
    case Tuple(elts) => "(" + elts.map(printTree(_)).reduce(_ ++ ", " ++ _) + ")"
    case List(elts) => "[" + elts.map(printTree(_)).reduce(_ ++ ", " ++ _) + "]"
    case NamedExpr(target, value) => f"${printTree(target)}:=${printTree(value)}"
    case GeneratorExp(elt, generators) =>
      f"(${printTree(elt)} ${printSeq(generators)})"
    case ListComp(elt, generators) => f"[${printTree(elt)} ${printSeq(generators)}]"
    case Comprehension(target, iter, ifs) =>
      val ifsStr = ifs.map(printTree(_)).map("if " + _).reduce(_ ++ " " ++ _)
      f"(for ${printTree(target)} in ${printTree(iter)} ${ifsStr})"
    case Yield(value) => f"(yield ${value.map(printTree(_)).getOrElse("")})"
    case YieldFrom(value) => f"(yield from ${printTree(value)})"
    case _ => "???"
  }
}