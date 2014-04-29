package project3c
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalaz.std.util.parsing.combinator.parser

import org.scalatest.FunSuite

import project3c.structures.ExprFactory._

class parseTests extends FunSuite {

  test("parsing correct expressions succeeds") {
    assert (ExprFParser.parseAll(ExprFParser.expr, "3").get == constant(3))
    assert (ExprFParser.parseAll(ExprFParser.expr, "x").get == variable("x"))
    assert (ExprFParser.parseAll(ExprFParser.expr, "lambda x . x").get == fun("x", variable("x")))
    assert (ExprFParser.parseAll(ExprFParser.expr, "λ x . x").get == fun("x", variable("x")))

    assert (ExprFParser.parseAll(ExprFParser.expr, "(1::nil)").get == cell(constant(1), constant(0)))
    assert (ExprFParser.parseAll(ExprFParser.expr, "(1::2)").get == cell(constant(1), constant(2)))
    assert (ExprFParser.parseAll(ExprFParser.expr, "(1::2::3)").get == cell(cell(constant(1), constant(2)), constant(3)))
    assert (ExprFParser.parseAll(ExprFParser.expr, "(1::2::3::4)").get ==
      cell(cell(cell(constant(1), constant(2)), constant(3)), constant(4)))
    
//    assert (ExprFParser.parseAll(ExprFParser.expr, "lambda x . x y z").get ==
//      fun("x", app(app(variable("x"), variable("y")), variable("z"))))
//    assert (ExprFParser.parseAll(ExprFParser.expr, "x y").get == app(variable("x"), variable("y")))

  }

}
