package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._
import edu.luc.cs.laufer.cs473.expressions.CombinatorParser.rep

object CombinatorParser extends JavaTokenParsers {
  def toplevel: Parser[Expr] =
    rep1(statement) ^^ { case a => Block(a: _*) }

  /** assignment  ::= ident "=" expression ";"*/
  def assignment: Parser[Expr] =
    ident ~ "=" ~ expr ~ ";" ^^ {
      case i ~ "=" ~ e ~ ";" => Assignment(Variable(i), e)
    }
  /** block ::= "{" statement* "}" */
  def block: Parser[Expr] =
    "{" ~ rep(statement) ~ "}" ^^ {
      case "{" ~ s ~ "}" => Block(s: _*)

    }

  /** conditional ::= "if" "(" expression ")" block [ "else" block ]*/
  def conditional: Parser[Expr] =
    "if" ~ "(" ~ expr ~ ")" ~ block ~! opt("else" ~ block) ^^ {
      case "if" ~ "(" ~ e ~ ")" ~ b1 ~ None              => Conditional(e, b1, Block())
      case "if" ~ "(" ~ e ~ ")" ~ b1 ~ Some("else" ~ b2) => Conditional(e, b1, b2)
    }
  /** loop  ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] =
    "while" ~! "(" ~ expr ~ ")" ~ block ^^ {
      case "while" ~ "(" ~ e ~ ")" ~ b => Loop(e, b)
    }

  /** statement   ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { case thing ~ _ => thing }
    | assignment
    | conditional
    | loop
    | block
  )
  /** factor ::= wholeNumber   |"+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | ident ~> factor ^^ { case s => Variable(s.toString) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
  )

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] = // use rep and seq
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ es => es.foldLeft(l) {
        case (r, e) => e match {
          case "+" ~ e1 => Plus(r, e1)
          case "-" ~ e1 => Minus(r, e1)
        }
      }
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ es => es.foldLeft(l) {
        case (r, e) => e match {
          case "*" ~ e1 => Times(r, e1)
          case "/" ~ e1 => Div(r, e1)
          case "%" ~ e1 => Mod(r, e1)
        }
      }
    }

}
