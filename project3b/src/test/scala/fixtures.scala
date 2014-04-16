package project3a

import project3a.behaviors._
import project3a.structures._
import project3a.structures.Constant
import project3a.structures.Fun
import project3a.structures.Iff
import project3a.structures.Var
import structures.ExprFactory._
import scalamu.In

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

  /* The Y-Combinator*/
  val Y = fun("G", app(fun("g", app(variable("G"), app(variable("g"), variable("g")))),
                                fun("g", app(variable("G"), app(variable("g"), variable("g"))))))
  val Ystring = "λG.(λg.G(gg)(λg.G(gg) "

  //def preFac(g: Int => Int)(n: Int): Int = if (n <= 0) 1 else n * g(n - 1)
  eval(app(app(fixtures.Y, fun("f", fun("n", iff(variable("n"),
    times(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))),
    constant(5)))
  val evalpreFacstring = "if (n <= 0) 1 else n * f(n - 1) "
  val YpreFacstring = " Y(preFac)(5) "

  //#3b
  val preLengthstring = "if (var 'c') 0 else 1+ f(c)"
  val preLength = fun("f", fun("c", iff(variable("c"), plus(constant(1),
                           app(variable("f"), tl(variable("c")))), constant(0)))) //TODO  please check??

  val preSize = 1 //TODO

}