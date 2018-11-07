package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._

  val simple1string = "x=5;"
  val simple1 = Assignment(Variable("x"), Constant(5))
  val simple2string = "x = 5 ; y = 7;"
  val simple2 = Assignment(Variable("x"), Constant(5)); Assignment(Variable("y"), Constant(7))

  val simple3string = "((1 + y2) - (3 * y4)) / 5;"
  val simple3 = Div(
    Minus(
      Plus(
        Constant(1),
        Variable("y2")
      ),
      Times(
        Constant(3),
        Variable("y4")
      )
    ),
    Constant(5)
  )

  val simple4string = "x = ((1 + y2) - (3 * y4)) / 5;"
  val simple4 =
    Assignment(
      Variable("x"),
      Div(
        Minus(
          Plus(
            Constant(1),
            Variable("y2")
          ),
          Times(
            Constant(3),
            Variable("y4")
          )
        ),
        Constant(5)
      )
    )

  val simple5string = "if (1) { x = 2; }"
  val simple5 =
    Conditional(
      Constant(1),
      Block(
        Assignment(
          Variable("x"),
          Constant(2)
        )

      ),
      Block()
    )

  val simple6string = "if (1) { x = 2; } else { x = 3; }"
  val simple6 =
    Conditional(
      Constant(1),
      Block(
        Assignment(
          Variable("x"),
          Constant(2)
        )
      ),
      Block(
        Assignment(
          Variable("x"),
          Constant(3)
        )
      )
    )

  val simple7string = "{ r = r + x; y = y + 1 ; }"
  val simple7 =
    Block(
      Assignment(
        Variable("r"),
        Plus(
          Variable("r"),
          Variable("x")
        )
      ),
      Assignment(
        Variable("y"),
        Plus(
          Variable("y"),
          Constant(1)
        )
      )
    )

  val simple8string = "if (4) { r = r + x; y = y + 1; }"
  val simple8 =
    Conditional(
      Constant(4),
      Block(
        Assignment(
          Variable("r"),
          Plus(
            Variable("r"),
            Variable("x")
          )
        ),
        Assignment(
          Variable("y"),
          Plus(
            Variable("y"),
            Constant(1)
          )
        )
      ),
      Block()
    )

  val simple9string = "while (y) { r = r + x; y = y - 1; }"
  val simple9 =
    Loop(
      Variable("y"),
      Block(
        Assignment(
          Variable("r"),
          Plus(
            Variable("r"),
            Variable("x")
          )
        ),
        Assignment(
          Variable("y"),
          Minus(
            Variable("y"),
            Constant(1)
          )
        )
      )
    )

  //  val simple10string = "while (y) { r = r + x ; y = y - 1 ;}"
  //  val simple10 =

  val complex1string = "((1 + 2) - (3 * 4)) / 5"
  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    )
  //  TODO how to test for indentation in Scala,
  // val complex1unparser =

  val complex1string2 = "  ((1 + 2) - (3 * 4)) / 5  "

  val complex2 =
    Mod(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          UMinus(
            Constant(3)
          ),
          Constant(4)
        )
      ),
      Constant(5)
    );

}
