package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._
import edu.luc.cs.laufer.cs473.expressions.CombinatorParser.rep

object CombinatorParser extends JavaTokenParsers {

  /** assignment  ::= ident "=" expression ";"*/
  def assignment: Parser[Expr] =
    ident ~ "=" ~ expr ~ ";" ^^ {
      case i ~ "=" ~ e ~ ";" => Assignment(i, e)
    } // TODO Q1: How do we not identify if as an ident?
  /** block ::= "{" statement* "}" */
  def block: Parser[Expr] =
    "{" ~! statement ~! "}" ^^ {
      case "{" ~! s ~! "}" => Block(s)
      // TODO Q2: Not really sure if we need Statement@_* or rep1(Statement) or
      // something else to represent the group of statements in a block
    }
  // TODO Q3 when to use !~ and when to use ~
  /** conditional ::= "if" "(" expression ")" block [ "else" block ]*/
  def conditional: Parser[Expr] =
    "if" ~! "(" ~ expr ~ ")" ~ block ~! rep("else" ~ block) ^^ {
      case "if" ~! "(" ~ e ~ ")" ~ b1                  => Conditional(e, b1, null)
      case "if" ~! "(" ~ e ~ ")" ~ b1 ~! ("else" ~ b2) => Conditional(e, b1, b2)
    }
  /** loop  ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] =
    "while" ~! "(" ~ expr ~ ")" ~ block ^^ {
      case "while" ~! "(" ~ e ~ ")" ~ b => Loop(e, b)
    } // TODO Q4 What is the difference between: a) expression of type doesn't conform to expected type Expr and b) Type mismatch, Expected: Expr, actual: any
  /** statement   ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = {
    expr ~ ";" ^^ { case thing => thing }
    | assignment ^^ { case thing => thing }
    | conditional ^^ { case thing => thing }
    | loop ^^ { case thing => thing }
    | block ^^ { case thing => thing }
  } // TODO Help to Fix the red
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
    term ~! rep(("+" | "-" | "=") ~ term) ^^ {
      case l           => l
      case l ~ "+" ~ r => Plus(l, r)
      case l ~ "-" ~ r => Minus(l, r)

    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case l           => l
      case l ~ "*" ~ r => Times(l, r)
      case l ~ "/" ~ r => Div(l, r)
      case l ~ "%" ~ r => Mod(l, r)
    }

}
