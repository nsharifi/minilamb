package project3a

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._

  test("misc tests") {
//    assert (eval(plus(variable("x"))))
  }

  test("reduce works") {
    assert(reduce(variable("x"), variable("x"), plus(constant(3), constant(4))) == plus(constant(3), constant(4)))
    assert(reduce(variable("y"), variable("x"), plus(constant(1), constant(2))) == plus(constant(1), constant(2)))
    assert(reduce(variable("y"), variable("x"), fun(variable("z"), plus(constant(1), constant(2)))) ==
      fun(variable("z"), plus(constant(1), constant(2))))
    assert(reduce(constant(10), variable("x"), plus(constant(1), constant(2))) == constant(10))
    assert(reduce(fun(variable("y"), constant(3)), variable("y"), constant(5)) == fun(variable("y"), constant(3)))
//    assert(reduce(fun(variable("y"), constant(3)), variable("x"), constant(5)) == fun(variable("y"), constant(3)))
    assert(reduce(plus(variable("x"), constant(2)), variable("x"), constant(3)) == plus(constant(3), constant(2)))

//    assert (reduce(fun(variable("x"), plus(variable("x"), constant(5))), variable("x"), constant(5)) == plus(constant(5), constant(5)))
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