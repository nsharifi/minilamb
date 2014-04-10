package project3a

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
  val Y = app(fun("G", fun("g", app(variable("G"), app(variable("g"), variable("g"))))),
    fun("g", app(variable("G"), app(variable("g"), variable("g")))))

//=========================PROJECT 3B

val preLength = fun("f", fun("c", iff(variable("c"), plus(constant(1),
                        app(variable("f"), tl(variable("c")))), constant(0)))) //TODO 6 please check??

    def preLengthA[B](g: List[B] => Int)(xs: List[B]) = xs match {
      case Nil => 0
      case _ :: ys => 1 + g(ys)
    }

}