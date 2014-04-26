package project3c
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
//import scalaz.std.util.parsing.combinator.parser

// for assert_=== to work on Int
import org.scalatest.FunSuite

import project3c.structures.ExprFactory._

class parseTests extends FunSuite {

  //    parser.parse("x").get assert_=== variable("x")
  //    parser.parse("lambda x . x").get assert_=== fun("x", variable("x"))
  //    parser.parse("lambda x . x y z").get assert_=== fun("x", app(app(variable("x"), variable("y")), variable("z")))
  //    parser.parse("λ x . x").get assert_=== fun("x", variable("x"))
  //    parser.parse("λ x . x y z").get assert_=== fun("x", app(app(variable("x"), variable("y")), variable("z")))
  //    parser.parse("(x y z)").get assert_=== app(app(variable("x"), variable("y")), variable("z"))
  test("parsing correct expressions succeeds") {
    assert (ExprFParser.parseAll(ExprFParser.expr, "3").get == constant(3))

  }

}
