package project3c

import project3c.behaviors._
import project3c.structures._
import project3c.structures.Constant
import project3c.structures.Fun
import project3c.structures.Iff
import project3c.structures.Var
import structures.ExprFactory._
import scalamu.In

object fixtures {

  /*simple arithmetic*/
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

  val cellComplex1 = cell(constant(10), cell(constant(20), cell(constant(30), constant(0))))

  val cellComplex2 = cell(cell(constant(10), constant(11)), cell(cell(constant(20),constant(21)),
    cell(cell(constant(30),constant(31)),constant(0))))


  /*numbers*/
  val two = constant(2)
  val three = constant(3)
  val five =constant(5)
  val six = constant(6)
  val onetwenty = constant(120)

  /* The Y-Combinator*/
  val Y = fun("G", app(fun("g", app(variable("G"), app(variable("g"), variable("g")))),
                                fun("g", app(variable("G"), app(variable("g"), variable("g"))))))/*pass*/
  val Ystring = "λG.(λg.G(gg)(λg.G(gg) "

  val preFact = fun("f", fun("n", iff(variable("n"),
    times(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))/* pass*/

  val evalpreFacstring = "if (n <= 0) 1 else n * f(n - 1) "
  val YpreFacstring = " Y(preFac)(5) "

  //#3b
  val preAdd = fun("f", fun("n", iff(variable("n"),
    plus(variable("n"), app(variable("f"), hd(variable("n")))), constant(0))))

  val preSum = fun("f", fun("n", iff(variable("n"),
    plus(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(0))))/*pass*/

  val preLength = fun("f", fun("c", iff(variable("c"),
                      plus(constant(1), app(variable("f"), tl(variable("c")))), constant(0))))/*pass*/

  val preSize = fun("f", fun("c", iff(iscell(variable("c")), plus(plus(constant(1), app(variable("f"),
    tl(variable("c")))), app(variable("f"), hd(variable("c")))), constant(0))))

}