package edu.luc.cs.laufer.cs473.expressions.ast

/** An initial algebra of arithmetic expressions. */

sealed trait Expr
case class Constant(value: Int) extends Expr
case class Variable(value: String) extends Expr
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr { require { (left != null) && (right != null) } }
case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Assignment(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Loop(left: Expr, right: Expr) extends BinaryExpr(left, right)

abstract class TrinaryExpr(left: Expr, center: Expr, right: Expr) extends Expr { require { (left != null) && (center != null) } }
case class Conditional(left: Expr, center: Expr, right: Expr) extends TrinaryExpr(left, center, right)

abstract class iterExpr(items: Expr*) extends Expr { require(items != null) }
case class Block(items: Expr*) extends iterExpr(items: _*)
