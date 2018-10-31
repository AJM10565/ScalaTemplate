package edu.luc.cs.laufer.cs473.expressions.ast

/** An initial algebra of arithmetic expressions. */


sealed trait Expr
case class Constant(value: Int) extends Expr
case class String_Expr(value: String) extends Expr
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr { require { (left != null) && (right != null) } }
case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Assignment(identifier: Expr, Assignedto: Expr)
case class Conditional(if_part: Expr, else_part: Expr)
abstract  class iterExpr(items :Expr* ) extends Expr {require(items != null, "Error: some shapes are null in this group")}
case class Block(expr: Expr) extends iterExpr(expr)