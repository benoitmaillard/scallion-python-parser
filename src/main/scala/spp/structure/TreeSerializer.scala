package spp.structure

import org.json4s._
import org.json4s.native.Document
import org.json4s.native.Serialization
import org.json4s.native.Serialization
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import java.io.{File, BufferedWriter, FileWriter}

import scala.language.implicitConversions

import AbstractSyntaxTree._
import scala.io.Source

/**
  * Tools to encode a syntax tree in JSON
  */
object TreeSerializer {
  private implicit def option2jvalue[A](opt: Option[A])(implicit ev: A => JValue): JValue = opt match {
    case Some(x) => x
    case None => JNull // a JNothing value is returned by default (hiding None values)
  }

  /**
    * Serialize a module in JSON
    *
    * @param module module to serialize
    * @return serialized module
    */
  def serialize(module: Module): Document = render(serializeNode(module))

  /**
    * Compares a module with a reference json file (that should be produced by CPython).
    * Both files are always saved under debug/ref.json and debug/output.json
    *
    * @param refPath path of the reference JSON tree
    * @param module module to compare
    */
  def compare(refPath: String, module: Module) = {
    val refJSON = Source.fromFile(refPath).mkString
    val ref = parse(refJSON)

    // hack to make sure values with None are not taken into account
    val output = parse(pretty(render(serializeNode(module))))

    val Diff(changed, added, deleted) = output diff ref

    saveJSON("debug/ref.json", ref)
    saveJSON("debug/output.json", output)

    println(pretty(serialize(module)))

    println(f"changed : $changed")
    println(f"added : $added")
    println(f"deleted : $deleted")
  }

  private def saveJSON(path: String, value: JValue) = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(pretty(render(value)))
    bw.close()
  }

  private def mkName(name: String): (String, String) = ("nodeName" -> name)

  private def splitCallArgs(rawArgs: Seq[CallArg]): (Seq[PosArg], Seq[KeywordArg]) =
    rawArgs.foldLeft(Seq.empty[PosArg], Seq.empty[KeywordArg]){
      case ((args, kwargs), cur) => cur match {
        case arg:PosArg => (args :+ arg, kwargs)
        case kwarg:KeywordArg => (args, kwargs :+ kwarg)
      }
    }

  private def mkOp(op: String): JValue = mkName(Map(
    "and" -> "And",
    "or" -> "Or",

    "+" -> "Add",
    "-" -> "Sub",
    "*" -> "Mult",
    "@" -> "MatMult",
    "/" -> "Div",
    "%" -> "Mod",
    "**" -> "Pow",
    "<<" -> "LShift",
    ">>" -> "RShift",
    "|" -> "BitOr",
    "^" -> "BitXor",
    "&" -> "BitAnd",
    "//" -> "FloorDiv",

    "+=" -> "Add",
    "-=" -> "Sub",
    "*=" -> "Mult",
    "/=" -> "Div",
    "//=" -> "FloorDiv",
    "%=" -> "Mod",
    "@=" -> "MatMult",
    "&=" -> "BitAnd",
    "|=" -> "BitOr",
    "^=" -> "BitXor",
    ">>=" -> "RShift",
    "<<=" -> "LShift",
    "**=" -> "Pow",

    "==" -> "Eq",
    "!=" -> "NotEq",
    "<" -> "Lt",
    "<=" -> "LtE",
    ">" -> "Gt",
    ">=" -> "GtE",
    "is" -> "Is",
    "is not" -> "IsNot",
    "in" -> "In",
    "not in" -> "NotIn",
  )(op))

  private def mkUnOp(op: String): JValue = mkName(Map(
    "~" -> "Invert",
    "not" -> "Not",
    "+" -> "UAdd",
    "-" -> "USub"
  )(op))

  private implicit def serializeSeq[A <: Tree](seq: Seq[A]): JArray = seq.map(serializeNode)

  /**
    * Creates a JSON object for a node
    *
    * @param node node to encode
    * @return JSON object representing the node
    */
  private implicit def serializeNode(node: Tree): JValue = node match {
    case Module(body) => mkName("Module") ~ ("body" -> serializeSeq(body))

    // statements
    case FunctionDef(name, args, body, decorators, returns, async) =>
      mkName(if (async) "AsyncFunctionDef" else "FunctionDef") ~
      ("name" -> name) ~
      ("args" -> args) ~ 
      ("body" -> body) ~ 
      ("decorator_list" -> decorators) ~ 
      ("returns" -> returns)
    case ClassDef(name, bases, body, decorators) => {
      val (args, keywords) = splitCallArgs(bases)
      mkName("ClassDef") ~
      ("name" -> name) ~ 
      ("bases" -> args.map(_.value)) ~
      ("keywords" -> keywords) ~
      ("body" -> body) ~
      ("decorator_list" -> decorators)
    }
    case Return(value) =>
      mkName("Return") ~ 
      ("value" -> value)
    case Delete(targets) => 
      mkName("Delete") ~ 
      ("targets" -> targets)
    case Assign(targets, value) => 
      mkName("Assign") ~ 
      ("targets" -> targets) ~ 
      ("value" -> value)
    case AugAssign(target, op, value) =>
      mkName("AugAssign") ~
      ("target" -> target) ~
      ("op" -> mkOp(op)) ~
      ("value" -> value)
    case AnnAssign(target, annotation, value, simple) =>
      mkName("AnnAssign") ~
      ("target" -> target) ~
      ("annotation" -> annotation) ~
      ("value" -> value) ~
      ("simple" -> (if (simple) 1 else 0))
    case For(target, iter, body, orelse, async) =>
      mkName(if (async) "AsyncFor" else "For") ~
      ("target" -> target) ~
      ("iter" -> iter) ~
      ("body" -> body) ~
      ("orelse" -> orelse)
    case While(test, body, orelse) =>
      mkName("While") ~
      ("test" -> test) ~
      ("body" -> body) ~
      ("orelse" -> orelse)
    case If(test, body, orelse) =>
      mkName("If") ~
      ("test" -> test) ~
      ("body" -> body) ~
      ("orelse" -> orelse)
    case With(items, body, async) =>
      mkName(if (async) "AsyncWith" else "With") ~
      ("items" -> items) ~
      ("body" -> body)
    case Raise(exc, cause) =>
      mkName("Raise") ~
      ("exc" -> exc) ~
      ("cause" -> cause)
    case Try(body, handlers, orelse, finalbody) =>
      mkName("Try") ~
      ("body" -> body) ~
      ("handlers" -> handlers) ~
      ("orelse" -> orelse) ~
      ("finalbody" -> finalbody)
    case Assert(test, msg) =>
      mkName("Assert") ~
      ("test" -> test) ~
      ("msg" -> msg)
    case Import(names) =>
      mkName("Import") ~
      ("names" -> names)
    case ImportFrom(module, names, level) =>
      mkName("ImportFrom") ~
      ("module" -> module) ~
      ("names" -> names) ~
      ("level" -> level.getOrElse(0))
    case Global(names) =>
      mkName("Global") ~
      ("names" -> names)
    case Nonlocal(names) =>
      mkName("Nonlocal") ~
      ("names" -> names)
    case ExprStmt(value) =>
      mkName("Expr") ~ 
      ("value" -> value)
    case Pass => mkName("Pass")
    case Break => mkName("Break")
    case Continue => mkName("Continue")
    
    // expressions
    case BoolOp(op, values) =>
      mkName("BoolOp") ~
      ("op" -> mkOp(op)) ~
      // values are represented as a list instead of left/right for some weird reason
      ("values" -> values)
    case NamedExpr(target, value) =>
      mkName("NamedExpr") ~
      ("target" -> target) ~
      ("value" -> value)
    case BinOp(op, left, right) =>
      mkName("BinOp") ~
      ("op" -> mkOp(op)) ~
      ("left" -> left) ~
      ("right" -> right)
    case UnaryOp(op, expr) =>
      mkName("UnaryOp") ~
      ("op" -> mkUnOp(op)) ~
      ("operand" -> expr)
    case Lambda(args, body) =>
      mkName("Lambda") ~
      ("args" -> args) ~
      ("body" -> body)
    case IfExpr(condition, ifValue, elseValue) =>
      mkName("IfExp") ~
      ("test" -> condition) ~
      ("body" -> ifValue) ~
      ("orelse" -> elseValue)
    case Dict(elts) => {
      val (keys, values) = elts.unzip(keyval => (keyval.key, keyval.value))
      mkName("Dict") ~
      ("keys" -> keys) ~
      ("values" -> values)
    }
    case Set(elts) =>
      mkName("Set") ~
      ("elts" -> elts)
    case ListComp(elt, generators) =>
      mkName("ListComp") ~
      ("elt" -> elt) ~
      ("generators" -> generators)
    case SetComp(elt, generators) =>
      mkName("SetComp") ~
      ("elt" -> elt) ~
      ("generators" -> generators)
    case DictComp(elt, generators) =>
      mkName("DictComp") ~
      ("key" -> elt.key) ~
      ("value" -> elt.value) ~
      ("generators" -> generators)
    case GeneratorExp(elt, generators) =>
      mkName("GeneratorExp") ~
      ("elt" -> elt) ~
      ("generators" -> generators)
    case Await(value) =>
      mkName("Await") ~
      ("value" -> value)
    case Yield(value) =>
      mkName("Yield") ~
      ("value" -> value)
    case YieldFrom(value) =>
      mkName("YieldFrom") ~
      ("value" -> value)
    case Compare(left, ops, comparators) =>
      mkName("Compare") ~
      ("left" -> left) ~
      ("ops" -> ops.map(mkOp(_))) ~
      ("comparators" -> comparators)
    case Call(func, allArgs) => {
      val (args, keywords) = splitCallArgs(allArgs)
      mkName("Call") ~
      ("func" -> func) ~
      ("args" -> args.map(_.value)) ~
      ("keywords" -> keywords)
    }
    case FormattedValue(value, conversion, format) =>
      mkName("FormattedValue") ~
      ("value" -> value) ~
      ("conversion" -> conversion.map(_.toInt).getOrElse(-1)) ~
      ("format_spec" -> format)
    case JoinedStr(values) =>
      mkName("JoinedStr") ~
      ("values" -> values)

    case IntConstant(value) =>
      mkName("Constant") ~
      ("type" -> "int") ~
      ("value" -> value)
    case FloatConstant(value) =>
      mkName("Constant") ~
      ("type" -> "float") ~
      ("value" -> value)
    case ImaginaryConstant(value) =>
      mkName("Constant") ~
      ("type" -> "complex") ~
      ("value" -> value)
    case StringConstant(value) =>
      mkName("Constant") ~ 
      ("type" -> "str") ~
      ("value" -> value)
    case BytesConstant(value) =>
      mkName("Constant") ~
      ("type" -> "bytes") ~
      ("value" -> value)
    case BooleanConstant(value) =>
      mkName("Constant") ~
      ("type" -> "bool") ~
      ("value" -> value)
    case NoneValue =>
      mkName("Constant") ~
      ("type" -> "NoneType") ~
      ("value" -> None) // TODO ?
    case Ellipsis =>
      mkName("Constant") ~
      ("type" -> "ellipsis") ~
      ("value" -> "...")
    
    case Attribute(value, attr) =>
      mkName("Attribute") ~
      ("value" -> value) ~
      ("attr" -> attr)
    case Subscript(value, slice) =>
      mkName("Subscript") ~
      ("value" -> value) ~
      ("slice" -> slice)
    case Starred(value) =>
      mkName("Starred") ~
      ("value" -> value)
    case Name(name) =>
      mkName("Name") ~
      ("id" -> name)
    case PythonList(elts) =>
      mkName("List") ~
      ("elts" -> elts)
    case Tuple(elts) =>
      mkName("Tuple") ~
      ("elts" -> elts)

    case Arguments(args, vararg, kwonly, kwarg) => {
      val defaults = args.map(_.default).filter(_.isDefined)
      val kwDefaults = kwonly.map(_.default)

      mkName("arguments") ~
      ("posonlyargs" -> Seq()) ~ // (posonly is not used)
      ("args" -> args) ~
      ("vararg" -> vararg) ~
      ("kwonlyargs" -> kwonly) ~
      ("kwarg" -> kwarg) ~
      ("kw_defaults" -> kwDefaults) ~
      ("defaults" -> defaults)
    }
    case Arg(arg, annotation, _) => // default is displayed at `Arguments` level
      mkName("arg") ~
      ("arg" -> arg) ~
      ("annotation" -> annotation)
    case KeywordArg(arg, value) =>            
      mkName("keyword") ~
      ("arg" -> arg.map {
        case Name(id) => id
        case other => throw new Error(f"argument key is not a name -> $other")
      }) ~ 
      ("value" -> value)
    case Comprehension(target, iter, ifs, async) =>
      mkName("comprehension") ~
      ("target" -> target) ~
      ("iter" -> iter) ~
      ("ifs" -> ifs) ~
      ("is_async" -> (if (async) 1 else 0))
    case ExceptionHandler(tpe, name, body) =>
      mkName("ExceptHandler") ~
      ("type" -> tpe) ~
      ("name" -> name) ~
      ("body" -> body)
    case Alias(name, asname) =>
      mkName("alias") ~
      ("name" -> name) ~
      ("asname" -> asname)
    case WithItem(contextExpr, optionalVars) =>
      mkName("withitem") ~
      ("context_expr" -> contextExpr) ~
      ("optional_vars" -> optionalVars)
    case DefaultSlice(lower, upper, step) =>
      mkName("Slice") ~
      ("lower" -> lower) ~
      ("upper" -> upper) ~
      ("step" -> step)
    case ExtSlice(dims) =>
      mkName("ExtSlice") ~
      ("dims" -> dims)
    case Index(value) =>
      mkName("Index") ~
      ("value" -> value)
  }
}