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

  val preSum = fun("f", fun("n", iff(variable("n"),
    plus(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(0))))

  val evalpreFacstring = "if (n <= 0) 1 else n * f(n - 1) "
  val YpreFacstring = " Y(preFac)(5) "

  //#3b
  val preLengthstring = "if (c) 0 else 1 + f(c)"

  val preLength = fun("f", fun("c", iff(variable("c"),
    plus(constant(1), app(variable("f"), eval(tl(variable("c"))))), constant(0))))
//  val preLength = fun("f", fun("c", iff(variable("c"),
//      plus(constant(1),app(variable("f"), tl(variable("c")))), constant(0)))) //TODO  please check??

//  cell(10, cell(20, cell(30, 0))) has size and length 3
//  example: cell(cell(10, 11), cell(cell(20, 21), cell(cell(30,31), 0))) has size 6 but length 3
  val cellComplex1 = cell(constant(10), cell(constant(20), cell(constant(30), constant(0))))

  val cellComplex2 =cell(cell(constant(10),constant(11)), cell(cell(constant(20),constant(21)),
                                        cell(cell(constant(30),constant(31)),constant(0))))

  val preSize = 1 //TODO

}