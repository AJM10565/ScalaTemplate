package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import behaviors._
import TestFixtures._
import ast._

object Main extends App {
  TesttoFormattedString(simple1)

  def TesttoFormattedString(expr: Expr) {
    println("p = " + expr)
    //println("evaluate(p) = " + evaluate(complex1))
    //    println("size(p) = " + size(expr))
    //    println("height(p) = " + height(expr))
    println(toFormattedString(expr))

  }
}

class Test extends FunSuite {
  test("evaluate(p)") { assert(evaluate(complex1) === -1) }
  test("size(p)") { assert(size(complex1) === 9) }
  test("height(p)") { assert(height(complex1) === 4) }
  test("evaluate(q)") { assert(evaluate(complex2) === 0) }
  test("size(q)") { assert(size(complex2) === 10) }
  test("height(q)") { assert(height(complex2) === 5) }
}

