package project3a

import structures.ExprFactory._

object fixtures {
  val complex1 =
    div(
      minus(
        plus(
          constant(1),
          constant(2)
        ),
        times(
          constant(3),
          constant(4)
        )
      ),
      constant(5)
    )

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex2 =
    mod(
      minus(
        plus(
          constant(1),
          constant(2)
        ),
        times(
          uminus(
            constant(3)
          ),
          constant(4)
        )
      ),
      constant(5)
    )

  val complex2string = "((1 + 2) - (-3 * 4)) % 5"
  val complex2bstring = "15 % 5"
}