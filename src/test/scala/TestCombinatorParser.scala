package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures._
import edu.luc.cs.laufer.cs473.expressions.behaviors.toFormattedString

object MainCombinatorParser extends App {
  //  val parsedExpr1 = CombinatorParser.parseAll(CombinatorParser.expr, simple1string)
  //  println(parsedExpr1.get)
  //  println(simple1)
  //  println(parsedExpr1.get == simple1)
  //  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, simple2string)
  //  println(parsedExpr2.get)
  //  println(simple2)
  //  println(parsedExpr2.get == simple2)
  //  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.expr, simple3string)
  //  println(parsedExpr3.get)
  //  println(simple3)
  //  println(parsedExpr3.get == simple3)
  //  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.expr, simple4string)
  //  println(parsedExpr4.get)
  //  println(simple4)
  //  println(parsedExpr4.get == simple4)
  //  val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.expr, simple5string)
  //  println(parsedExpr5.get)
  //  println(simple5)
  //  println(parsedExpr5.get == simple5)
  //  val parsedExpr6 = CombinatorParser.parseAll(CombinatorParser.expr, simple6string)
  //  println(parsedExpr6.get)
  //  println(simple6)
  //  println(parsedExpr6.get == simple6)
  //  val parsedExpr7 = CombinatorParser.parseAll(CombinatorParser.expr, simple7string)
  //  println(parsedExpr7.get)
  //  println(simple7)
  //  println(parsedExpr7.get == simple7)
  //  val parsedExpr8 = CombinatorParser.parseAll(CombinatorParser.expr, simple8string)
  //  println(parsedExpr8.get)
  //  println(simple8)
  //  println(parsedExpr8.get == simple8)
  //  val parsedExpr9 = CombinatorParser.parseAll(CombinatorParser.expr, simple9string)
  //  println(parsedExpr9.get)
  //  println(simple9)
  //  println(parsedExpr9.get == simple9)
  //  val parsedExprc1 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  //  println(parsedExprc1.get)
  //  println(complex1)
  //  println(parsedExprc1.get == complex1)
  //  val parsedExprc2 = CombinatorParser.parseAll(CombinatorParser.expr, complex2string)
  //  println(parsedExprc2.get)
  //  println(complex2)
  //  println(parsedExprc2.get == complex2)

}

class TestCombinatorParser extends FunSuite {

  test("parser(simple1)") { getparsed(simple1string) === simple1 }
  test("parser(simple1)") { getparsed(simple2string) === simple2 }
  test("parser(simple1)") { getparsed(simple3string) === simple3 }
  test("parser(simple1)") { getparsed(simple4string) === simple4 }
  test("parser(simple1)") { getparsed(simple5string) === simple5 }
  test("parser(simple1)") { getparsed(simple6string) === simple6 }
  test("parser(simple1)") { getparsed(simple7string) === simple7 }
  test("parser(simple1)") { getparsed(simple8string) === simple8 }
  test("parser(simple1)") { getparsed(simple9string) === simple9 }
  test("parser(simple1)") { getparsed(complex1string) === complex1 }
  test("parser(simple1)") { getparsed(complex2string) === complex2 }

  def getparsed(string: String): String = {
    toFormattedString(CombinatorParser.parseAll(CombinatorParser.toplevel, string).get)(false)
  }
}
