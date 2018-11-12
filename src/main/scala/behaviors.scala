package edu.luc.cs.laufer.cs473.expressions

import ast._
import scala.util.{Failure, Success, Try}
import scala.collection.mutable.{ Map => MutableMap }

object behaviors {

  /** Unused Code Portion */
  //  /** A cell (l-value) for storing a value. */
  //  case class Cell(var value: Int) {
  //    def get: Int = value
  //    def set(value: Int): Cell = { this.value = value; this }
  //  }


  /** A object (instance) is a mapping from variable names to storage cells. */
  type Instance = MutableMap[String, Value]

  /** A memory store is a special top-level object (instance). */
  type Store = Instance

  /** A run-time value is always a number for now. We represent NULL as 0. */
  sealed trait Value

  case class Num(value: Int) extends Value


  /** A companion object defining a useful Value instance. */
  object Value {
    val NULL = Num(0)
  }

  /** The result of a successful or failed computation. */
  type Result = Try[Value]


  def newstore: Store = MutableMap.empty[String, Value]

  /** An interpreter for expressions and statements. */

  object Execute {

    def apply(store: Store)(s: Expr): Result = s match { // Do X given a particular "store" AKA MutableMap and input expr

      case Constant(value) => Success(Num(value)) // We use Num(Value) because the output type of a execute computation is of type Try[Value] See above

      case UMinus(left) => for {l <- apply(store)(left)} yield Num(-l.asInstanceOf[Num].value) // We use this 'for'->'yield' wrapper as a way to catch errors
      // we are specifically worried about errors in the manipulation of the MutableMap, which is why for Constant above, we don't need this wrapper

      // The Next 5 cases all follow the same pattern for binary arithmetic operation: The Map manipulations occur inside the for, and on success we yield the appropriate linear combination
      case Plus(left, right) => for {
        l <- apply(store)(left)
        r <- apply(store)(right)
      } yield Num(l.asInstanceOf[Num].value + r.asInstanceOf[Num].value)

      case Minus(left, right) => for {
        l <- apply(store)(left)
        r <- apply(store)(right)
      } yield Num(l.asInstanceOf[Num].value - r.asInstanceOf[Num].value)

      case Times(left, right) => for {
        l <- apply(store)(left)
        r <- apply(store)(right)
      } yield Num(l.asInstanceOf[Num].value * r.asInstanceOf[Num].value)

      case Div(left, right) => for {
        l <- apply(store)(left)
        r <- apply(store)(right)
      } yield Num(l.asInstanceOf[Num].value / r.asInstanceOf[Num].value)

      case Mod(left, right) => for {
        l <- apply(store)(left)
        r <- apply(store)(right)
      } yield Num(l.asInstanceOf[Num].value % r.asInstanceOf[Num].value)

      case Variable(name) => Try(store(name))

      case Assignment(left, right) => {
        for {
          rvalue <- apply(store)(right)
        } yield {
          store.put(left.asInstanceOf[String], rvalue) // changed to left.asInstanceof[String] still needs testing
          Value.NULL
        }
      }

      case Block(statements@_*) => {

        def doSequence(blockstatements: Seq[Expr]): Result ={
        // Neither option gives the correct return type
//          //Option #1
//          statements.foldLeft(Value.NULL.asInstanceOf[Value])((c, s) => apply(store)(s))

          //Option #2 copying pattern from https://github.com/lucproglangcourse/simpleimperative-algebraic-scala/blob/master/src/main/scala/evaluate.scala
          val i = statements.iterator
          var result = Value.NULL.asInstanceOf[Value]
          while (i.hasNext) {
            i.next( )
            for { // Using the for to try and catch an error
              statement_instance <- i.hasNext
              executedstatementoutput <- apply(store)(statement_instance)
            } yield {
              Success(executedstatementoutput)
              Value.NULL
            }
           }

          }

          doSequence(statements)

        }








//

//      case Block(expressions) =>
//        // TODO http://stackoverflow.com/questions/12892701/abort-early-in-a-fold
//        def doSequence: Result = {
//          val i = expressions.iterator
//          var result: Cell = Cell.NULL
//          while (i.hasNext) {
//            i.next().eval match {
//              case Success(r)     => result = r
//              case f @ Failure(_) => return f
//            }
//          }
//          Success(result)
//        }
//        thunk { doSequence }


      case Loop(guard, body) =>
        var gvalue = apply(store)(guard) // Evaluate the guard
        while (gvalue.asInstanceOf[Int] != 0) {
          apply(store)(body)
          gvalue = apply(store)(guard)
        }
        Success(Value.NULL) // Still Don't Understand why sometimes we use Success and sometimes not


    }
  }

        def size(e: Expr): Int = e match { // number of any type of nodes
          case Constant(c) => 1
          case UMinus(r) => 1 + size(r)
          case Plus(l, r) => 1 + size(l) + size(r)
          case Minus(l, r) => 1 + size(l) + size(r)
          case Times(l, r) => 1 + size(l) + size(r)
          case Div(l, r) => 1 + size(l) + size(r)
          case Mod(l, r) => 1 + size(l) + size(r)
        }

        def height(e: Expr): Int = e match { // how tall is the tree
          case Constant(c) => 1
          case UMinus(r) => 1 + height(r)
          case Plus(l, r) => 1 + math.max(height(l), height(r))
          case Minus(l, r) => 1 + math.max(height(l), height(r))
          case Times(l, r) => 1 + math.max(height(l), height(r))
          case Div(l, r) => 1 + math.max(height(l), height(r))
          case Mod(l, r) => 1 + math.max(height(l), height(r))
        }

        def toFormattedString(prefix: String)(e: Expr)(bool: Boolean): String = e match {

          // !bool is the AST "UGLY" Style
          case Constant(c) if !bool => prefix + c.toString
          case Variable(value) if !bool => prefix + value
          case UMinus(r) if !bool => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Plus(l, r) if !bool => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Minus(l, r) if !bool => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Times(l, r) if !bool => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Div(l, r) if !bool => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Mod(l, r) if !bool => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Assignment(l, r) if !bool => buildExprString(prefix, "Assignment", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Loop(l, r) if !bool => buildExprString(prefix, "Loop", toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Conditional(l, c, r) if !bool => buildtriExprString(prefix, "Conditional", toFormattedString(prefix + INDENT)(c)(bool), toFormattedString(prefix + INDENT)(l)(bool), toFormattedString(prefix + INDENT)(r)(bool))(bool)
          case Block(strings@_*) if !bool => build_infinite_ExprString(prefix, strings)(bool)

          // bool is the "Pretty-Print" Style
          case Constant(c) if bool => c.toString
          case Variable(value) if bool => value
          case UMinus(r) if bool => buildUnaryExprString("", "-", toFormattedString(prefix)(r)(bool))(bool)
          case Plus(l, r) if bool => buildExprString("", "+", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Minus(l, r) if bool => buildExprString("", "-", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Times(l, r) if bool => buildExprString("", "*", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Div(l, r) if bool => buildExprString("", "/", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Mod(l, r) if bool => buildExprString("", "%", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Assignment(l, r) if bool => buildAssignString("", "=", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Loop(l, r) if bool => buildwhileString("", toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Conditional(l, c, r) if bool => buildtriExprString("", "if", toFormattedString(prefix)(c)(bool), toFormattedString("")(l)(bool), toFormattedString("")(r)(bool))(bool)
          case Block(strings@_*) if bool => build_infinite_ExprString(prefix, strings)(bool)

        }

        def toFormattedString(e: Expr)(bool: Boolean): String = toFormattedString("")(e)(bool)

        def buildwhileString(prefix: String, leftString: String, rightString: String)(bool: Boolean): String = {
          val result = new StringBuilder(prefix)
          result.append("while")
          result.append("(")
          result.append(leftString)
          result.append(")")
          result.append(prefix)
          result.append(rightString)
          result.toString
        }

        def build_infinite_ExprString(prefix: String, nodeExprs: Seq[Expr])(bool: Boolean): String = {
          val result = new StringBuilder(prefix)

          if (!bool) {

            val strings: Seq[String] = nodeExprs.map(expr => toFormattedString(prefix)(expr)(bool))
            strings.foreach(string => result.append(string))

          } else {
            var tabspace = prefix + "  "

            result.append("{")
            result.append(EOL)

            val strings: Seq[String] = nodeExprs.map(expr => toFormattedString(tabspace)(expr)(bool))

            strings.foreach {
              string =>
                result.append(tabspace)
                result.append(string)
                result.append(EOL)
            }
            result.append(prefix)
            result.append("}")

          }
          result.toString
        }

        def buildAssignString(prefix: String, nodeString: String, leftString: String, rightString: String)(bool: Boolean) = {
          val result = new StringBuilder(prefix)
          result.append(leftString)
          result.append(" ")
          result.append(nodeString)
          result.append(" ")
          result.append(rightString)
          result.append(";")
          result.toString
        }

        def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String)(bool: Boolean) = {
          val result = new StringBuilder(prefix)
          if (!bool) {
            result.append(nodeString)
            result.append("(")
            result.append(EOL)
            result.append(leftString)
            result.append(", ")
            result.append(EOL)
            result.append(rightString)
            result.append(")")
          } else {
            result.append("(")
            result.append(leftString)
            result.append(" ")
            result.append(nodeString)
            result.append(" ")
            result.append(rightString)
            result.append(")")

          }

          result.toString
        }

        def buildtriExprString(prefix: String, nodeString: String, leftString: String, centerString: String, rightString: String)(bool: Boolean) = {
          val result = new StringBuilder(prefix)
          if (!bool) {
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
          } else {

            result.append(nodeString)
            result.append(" ")
            result.append("(")
            result.append(centerString)
            result.append(")")
            result.append(leftString)
            result.append("else")

            result.append(rightString)

          }

          result.toString
        }

        def buildUnaryExprString(prefix: String, nodeString: String, exprString: String)(bool: Boolean) = {
          val result = new StringBuilder(prefix)

          if (!bool) {

            result.append(nodeString)
            result.append("(")
            result.append(EOL)
            result.append(exprString)
            result.append(")")

          } else {
            result.append(nodeString)
            // We don't need a ( because this isn't the ast
            // Similarly we don't need an EOL
            result.append(exprString)
            // Similarly we don't need a )
            result.append(";")

          }
          result.toString

        }

        val EOL = scala.util.Properties.lineSeparator
        val INDENT = ".."



}
