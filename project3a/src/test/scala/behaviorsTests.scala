package project3a

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._

  test("misc tests") {
//    assert (eval(plus(variable("x"))))
  }

  test("reduce works") {
    assert(reduce(variable("x"), plus(constant(3), constant(4)), variable("x")) == plus(constant(3), constant(4)))
    assert(reduce(variable("y"), plus(constant(1), constant(2)), variable("x")) == plus(constant(1), constant(2)))
    assert(reduce(variable("y"), fun(variable("z"), plus(constant(1), constant(2))), variable("x")) ==
      fun(variable("z"), plus(constant(1), constant(2))))
    assert(reduce(constant(10), plus(constant(1), constant(2)), variable("x")) == constant(10))
    assert(reduce(fun(variable("y"), constant(3)), constant(5), variable("y")) == fun(variable("y"), constant(3)))
    assert(reduce(fun(variable("y"), constant(3)), constant(5), variable("x")) != fun(variable("y"), constant(3)))
    assert(reduce(plus(variable("x"), constant(2)), constant(3), variable("x")) == plus(constant(3), constant(2)))
  }

  test("eval arithmetic works") {
    assert(eval(fixtures.twoPlusthree) == constant(5))
    assert(eval(fixtures.threeMinusone) == constant(2))
    assert(eval(fixtures.threeTimestwo) == constant(6))
  }

  test("eval part A works") {
    assert(eval(constant(3)) == constant(3))
    assert(eval(variable("x")) == err("Variable"))
    assert(eval(fun(variable("x"), plus(constant(7), variable("x")))) == fun(variable("x"), plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
    assert(eval(app(fun(variable("x"), plus(constant(7), variable("x"))), constant(3))) == constant(10))
  }

  test("eval part B works") {
    assert(eval( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(eval(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(eval(iff(variable("x"), constant(3), constant(4)) ) == err("Var Conditional"))
    assert(eval(iff(fun(variable("x"), variable("x")), constant(3), constant(4))) == constant(3))
    assert(eval(iff(fun(variable("x"), variable("y")), constant(3), constant(4))) == constant(3))
  }

  test("eval part C  works") {
    assert(eval(fun(variable("x"), plus(constant(7), variable("x")))) == fun(variable("x"), plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
  }

  test("eval Y works") {
    assert(eval(app(app(fixtures.Y, fun(variable("f"), fun(variable("n"), iff(variable("n"),times(variable("n"),
      app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))), constant(5))) == constant(120)) //err("Application of Non-Function"))
  }

  test("variable generator works") {assert (nextVar != nextVar) }/* Two calls to nextVar generate different values*/

}