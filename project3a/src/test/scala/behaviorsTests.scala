package project3a

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._

  test("misc tests") {
//    assert (eval(plus(variable("x"))))
  }

  test("reduce works") {
    assert(reduce(variable("x"), "x", plus(constant(3), constant(4))) == plus(constant(3), constant(4)))
    assert(reduce(variable("y"), "x", plus(constant(1), constant(2))) == plus(constant(1), constant(2)))
    assert(reduce(variable("y"), "x", fun("z", plus(constant(1), constant(2)))) ==
      fun("z", plus(constant(1), constant(2))))
    assert(reduce(constant(10), "x", plus(constant(1), constant(2))) == constant(10))
    assert(reduce(fun("y", constant(3)), "y", constant(5)) == fun("y", constant(3)))
    assert(reduce(fun("y", constant(3)), "x", constant(5)) == fun("y1", constant(3)))
    assert(reduce(plus(variable("x"), constant(2)), "x", constant(3)) == plus(constant(3), constant(2)))

    assert(reduce(fun("y", plus(plus(variable("y"), app(fun("x", variable("x")), constant(3))), variable("x"))), "x", variable("y")) ==
      fun("y1", plus(plus(variable("y1"), app(fun("x", variable("x")), constant(3))), variable("y"))))
  }

  test("Alpha substitution") {
    assert (reduce(fun("y", app(variable("x"), fun("x", plus(variable("x"), variable("y"))))), "x", variable("y")) ==
      fun("y1", app(variable("y"), fun("x", plus(variable("x"), variable("y1"))))))
//    assert (reduce(fun("x", fun("y", app(variable("x"), fun("x", plus(variable("x"), variable("y")))))), "x", variable("y")) ==
//      fun("y1", app(variable("y"), fun("x", plus(variable("x"), variable("y1"))))))
  }



  test("eval arithmetic works") {
    assert(eval(fixtures.twoPlusthree) == constant(5))
    assert(eval(fixtures.threeMinusone) == constant(2))
    assert(eval(fixtures.threeTimestwo) == constant(6))
  }

  test("eval part A works") {
    assert(eval(constant(3)) == constant(3))
//    assert(eval(variable("x")) == err("Variable"))
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
    assert(eval(app(fun("x", plus(constant(7), variable("x"))), constant(3))) == constant(10))
  }

  test("eval part B works") {
    assert(eval( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(eval(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(eval(iff(variable("x"), constant(3), constant(4)) ) == err("Var Conditional"))
    assert(eval(iff(fun("x", variable("x")), constant(3), constant(4))) == constant(3))
    assert(eval(iff(fun("x", variable("y")), constant(3), constant(4))) == constant(3))
  }

  test("eval part C  works") {
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
  }



  test("eval Y works") {
    assert(eval(app(app(fixtures.Y, fun("f", fun("n", iff(variable("n"),
      times(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))),
      constant(5))) == constant(120)) //err("Application of Non-Function"))
  }

  test("variable generator works") {assert (nextVar != nextVar) }/* Two calls to nextVar generate different values*/

}