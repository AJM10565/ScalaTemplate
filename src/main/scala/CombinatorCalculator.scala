package edu.luc.cs.laufer.cs473.expressions

import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder

object CombinatorCalculator extends App {

  val terminal = TerminalBuilder.terminal
  val reader = LineReaderBuilder.builder.terminal(terminal).build
  val prompt = "myprompt> "
  var store = behaviors.newstore
  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.toplevel, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val expr = result.get
      println("The parsed statements are: ")
      println(toFormattedString(expr)(false))
      println("The unparsed statements are:")
      println(toFormattedString(expr)(true))

      // println("It has size " + size(expr) + " and height " + height(expr)) // Still Doesn't work
      println("Memory: " + store)
      println("It evaluates to " + Execute(store)(expr))
      println("Memory: " + store)
    }
  }

  try {
    reader.readLine(prompt)
  } catch {
    case e: Exception => e.printStackTrace()
  }

  // needs a try catch
  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines foreach { line =>
      processExpr(line)
      print("Enter infix expression: ")
    }
  }
}
