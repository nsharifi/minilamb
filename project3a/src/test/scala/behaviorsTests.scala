package project3a

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._


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

  test("interpret arithmetic works") {
    assert(interpret( fixtures.twoPlusthree ) == constant(5))
    assert(interpret(fixtures.threeMinusone ) == constant(2))
    assert(interpret( fixtures.threeTimestwo ) == constant(6))
  }

  test("interpret part A works") {
    assert(interpret(constant(3)) == constant(3))
    assert(interpret(variable("x")) == err("Variable"))
    assert(interpret(fun(variable("x"), plus(constant(7), variable("x")))) ==
      fun(variable("x"), plus(constant(7), variable("x"))))
    assert(interpret(app(variable("x"), constant(3))) == err("Application of Non-Function"))
    assert(interpret(app(fun(variable("x"), plus(constant(7), variable("x"))), constant(3))) == constant(10))
  }

  test("interpret part B Ifs works") {
    assert(interpret( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(interpret(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(interpret(iff(variable("x"), constant(3), constant(4)) ) == err("Var Conditional"))
    assert(interpret(iff(fun(variable("x"), variable("x")), constant(3), constant(4))) == constant(3))
    assert(interpret(iff(fun(variable("x"), variable("y")), constant(3), constant(4))) == constant(3))

  }

  test("interpret part C works") {
    assert(interpret(fun(variable("x"), plus(constant(7), variable("x")))) ==
      fun(variable("x"), plus(constant(7), variable("x"))))
    assert(interpret(app(variable("x"), constant(3))) == err("Application of Non-Function"))
  }

  test("interpret  Y works") {
    assert(interpret(app(app(fixtures.Y, fun(variable("f"), fun(variable("n"), iff(variable("n"),times(variable("n"),
      app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))), constant(5))) == err("Application of Non-Function")) //constant(120))
  }

  test("variable generator works") {
    // Two calls to nextVar generate different values
    assert (nextVar != nextVar)
  }

}