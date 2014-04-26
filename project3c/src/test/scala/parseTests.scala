package project3c
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
//import scalaz.std.util.parsing.combinator.parser

// for assert_=== to work on Int
import org.scalatest.FunSuite

import project3c.structures.ExprFactory._

class parseTests extends FunSuite {

  test("parsing correct expressions succeeds") {
    val parsedExpr = ExprFParser.parseAll(ExprFParser.expr, "3");

//    import scalamu._

    assert (parsedExpr.get == constant(3))
//    parser.parseAll("3").get assert_=== constant(3)
  }

}
