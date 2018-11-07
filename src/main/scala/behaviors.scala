package edu.luc.cs.laufer.cs473.expressions

import ast._

object behaviors {

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)
  }

  def size(e: Expr): Int = e match { // number of any type of nodes
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
  }

  def height(e: Expr): Int = e match { // how tall is the tree
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
  }

  def toFormattedString(prefix: String)(e: Expr)(bool: Boolean): String = e match {

    // !bool is the AST "UGLY" Style
    case Constant(c) if !bool          => prefix + c.toString
    case Variable(value) if !bool      => prefix + value
    case UMinus(r) if !bool            => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Plus(l, r) if !bool           => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Minus(l, r) if !bool          => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Times(l, r) if !bool          => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Div(l, r) if !bool            => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Mod(l, r) if !bool            => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Assignment(l, r) if !bool     => buildExprString(prefix, "Assignment", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Loop(l, r) if !bool           => buildExprString(prefix, "Loop", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Conditional(l, c, r) if !bool => buildtriExprString(prefix, "Conditional", toFormattedString(prefix + INDENT)(c)(bool), toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
    case Block(strings @ _*) if !bool  => build_infinite_ExprString(prefix, strings)(bool)

    // bool is the "Pretty-Print" Style
    case Constant(c) if bool           => prefix + c.toString
    case Variable(value) if bool       => prefix + value
    //        case UMinus (r)             if  bool     => buildUnaryExprString (prefix, "UMinus", toFormattedString (prefix + INDENT) (r) )
    //        case Plus (l, r)            if  bool     => buildExprString (prefix, "Plus", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Minus (l, r)           if  bool     => buildExprString (prefix, "Minus", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Times (l, r)           if  bool     => buildExprString (prefix, "Times", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Div (l, r)             if  bool     => buildExprString (prefix, "Div", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Mod (l, r)             if  bool     => buildExprString (prefix, "Mod", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Assignment (l, r)      if  bool     => buildExprString (prefix, "Assignment", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Loop (l, r)            if  bool     => buildExprString (prefix, "Loop", toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    //        case Conditional (l, c, r)  if  bool     => buildtriExprString (prefix, "Conditional", toFormattedString (prefix + INDENT) (c), toFormattedString (prefix + INDENT) (l), toFormattedString (prefix + INDENT) (r) )
    case Block(strings @ _*) if bool   => build_infinite_ExprString(prefix, strings)(bool)

  }

  def toFormattedString(e: Expr)(bool: Boolean): String = toFormattedString("")(e)(bool)

  def build_infinite_ExprString(prefix: String, nodeExprs: Seq[Expr])(bool: Boolean): String = {
    if (!bool) {
      val result = new StringBuilder(prefix)
      val strings: Seq[String] = nodeExprs.map(expr => toFormattedString(prefix)(expr)(bool))
      strings.foreach(string => result.append(string))
      result.toString
    } else {
      val result = new StringBuilder(prefix)
      val strings: Seq[String] = nodeExprs.map(expr => toFormattedString(prefix + "  ")(expr)(bool))
      strings.foreach(string => result.append(string))
      result.toString
    }
  }

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String)(bool: Boolean) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }
  def buildtriExprString(prefix: String, nodeString: String, leftString: String, centerString: String, rightString: String)(bool: Boolean) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(centerString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String)(bool: Boolean) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
