package project3a

import project3a.structures.Constant
import structures.ExprFactory._

object fixtures {

  val twoPlusthree = plus(
                      constant(2),
                      constant(3)
                    )
  val twoPlusthreestring = "2 + 3"

  val threeMinusone = minus(
                       constant(3),
                       constant(1)
                     )
  val threeMinusonestring = "3-1"

  val threeTimestwo = times(
                        constant(3),
                        constant(2)
                      )
  val threeTimestwostring = "3*2"

  // The Y-Combinator
  val Y = app(fun(variable("G"), fun(variable("g"), app(variable("G"), app(variable("g"), variable("g"))))),
    fun(variable("g"), app(variable("G"), app(variable("g"), variable("g")))))



}