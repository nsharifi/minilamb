package project3c

import org.scalatest.FunSuite

import project3c.structures.ExprFactory._
import ExprFParser.parse

class parseTests extends FunSuite {

  test("parsing correct expressions succeeds") {
    assert (parse("3").get == constant(3))
    assert (parse("x").get == variable("x"))
    assert (parse("lambda x . x").get == fun("x", variable("x")))
    assert (parse("λ x . x").get == fun("x", variable("x")))

    assert (parse("(1::nil)").get == cell(constant(1), constant(0)))
    assert (parse("(1::2)").get == cell(constant(1), constant(2)))
    assert (parse("(1::2::3)").get == cell(cell(constant(1), constant(2)), constant(3)))
    assert (parse("(1::2::3::4)").get ==
      cell(cell(cell(constant(1), constant(2)), constant(3)), constant(4)))

    assert (parse("lambda x . x y z").get ==
      fun("x", app(app(variable("x"), variable("y")), variable("z"))))
    assert (parse("λ x . x y z").get ==
      fun("x", app(app(variable("x"), variable("y")), variable("z"))))

    assert (parse("(x y z)").get ==
      app(app(variable("x"), variable("y")), variable("z")))/*pass*/

    // Function currying
    assert (parse("λ x1 x2 x3 . (x1+x2)+x3").get ==
      fun("x1", fun("x2", fun("x3", plus(plus(variable("x1"), variable("x2")), variable("x3"))))) )

    assert (parse("let x=e in f").get == app(fun("x", variable("f")), variable("e")))
    assert (parse("let x1=e1 x2=e2 in e").get ==
      app(app(fun("x1", fun("x2", variable("e"))), variable("e2")), variable("e1")) )
//    assert (parse("let x1=e1 x2=e2 x3=e3 in e").get ==
//      app(app(app(fun("x1", fun("x2", fun("x3", variable("e")))), variable("e3")), variable("e2")), variable("e1")) )
  }

  test("parsing incorrect expressions fails") {
    assert (parse("x y z").isEmpty)
    assert (parse("x λ z").isEmpty)
  }

}/*parseTests*/
