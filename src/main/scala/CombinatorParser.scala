package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** Assignment ::= ident "=" expression ";" */
  def assignment: Parser[Expr] =
    ident ~ "=" ~expr ~ ";" ^^ {
      case l ~ "=" ~ r ~ ";" => Assignment(l,r)
    } // How do we not identify if as an ident?

  def block: Parser[Expr] =
    "{" ~! statement ~! "}" ^^ {
      case "{" ~! s ~! "}" => Block(s)
        // Not really sure if we need Statement@_* or rep1(Statement) or
      // something else to represent the group of statements in a block
    }

  def conditional: Parser[Expr]=
    "if" 






  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] = // use rep and seq
    term ~! rep(("+" | "-"|"=") ~ term) ^^ {
      case l        => l
      case l ~ "+" ~ r => Plus(l, r)
      case l ~ "-" ~ r => Minus(l, r)

    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case l           => l
      case l ~"*" ~ r => Times(l, r)
      case l ~ "/" ~ r => Div(l, r)
      case l ~ "%" ~ r => Mod(l, r)
    }

  /** factor ::= wholeNumber   |"+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | ident ~> factor  ^^ {case s => String_Expr(s.toString)}
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
  )






}
