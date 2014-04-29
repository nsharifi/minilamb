package project3c
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalaz.std.util.parsing.combinator.parser

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
  }

  test("parsing incorrect expressions fails") {
    assert (parse("x y z").isEmpty)
  }

}/*parseTests*/

