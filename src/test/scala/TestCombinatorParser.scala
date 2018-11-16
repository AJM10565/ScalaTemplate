package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures._
import edu.luc.cs.laufer.cs473.expressions.behaviors.toFormattedString

class TestCombinatorParser extends FunSuite {

  test("parser(simple1)")  {getparsed(simple1string) === simple1 }
  test("evaluate(simple1)"){true } //do something
  test("parser(simple2)") { getparsed(simple2string) === simple2 }
  test("parser(simple3)") { getparsed(simple3string) === simple3 }
  test("parser(simple4)") { getparsed(simple4string) === simple4 }
  test("parser(simple5)") { getparsed(simple5string) === simple5 }
  test("parser(simple6)") { getparsed(simple6string) === simple6 }
  test("parser(simple7)") { getparsed(simple7string) === simple7 }
  test("parser(simple8)") { getparsed(simple8string) === simple8 }
  test("parser(simple9)") { getparsed(simple9string) === simple9 }


  def getparsed(string: String): String = {
    toFormattedString(CombinatorParser.parseAll(CombinatorParser.toplevel, string).get)(false)
  }
}
