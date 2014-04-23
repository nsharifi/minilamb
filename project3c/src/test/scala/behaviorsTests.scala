package project3c

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._
  import fixtures._

  test("reduce works") {
    assert(reduce(variable("x"), "x", plus(constant(3), constant(4))) == plus(constant(3), constant(4)))
    assert(reduce(variable("y"), "x", plus(constant(1), constant(2))) == variable("y"))
    assert(reduce(variable("y"), "x", fun("z", plus(constant(1), constant(2)))) == variable("y"))
    assert(reduce(constant(10), "x", plus(constant(1), constant(2))) == constant(10))
    assert(reduce(fun("y", constant(3)), "y", constant(5)) == fun("y", constant(3)))
    assert(reduce(plus(variable("x"), constant(2)), "x", constant(3)) == plus(constant(3), constant(2)))
    assert(reduce(fun("y", variable("y")), "x", constant(5)) == fun("y1", variable("y1")))
    assert (reduce(fun("y", plus(variable("y"), plus(app(fun("x", variable("x")), constant(3)), variable("x")))), "x", constant(5)) ==
      fun("y2", plus(variable("y2"), plus(app(fun("y4", variable("y4")), constant(3)), constant(5))) ))
  }

  test("eval part A works") {
    assert(eval(constant(3)) == constant(3))
    assert(eval(variable("x")) == err("Variable"))
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
    assert(eval(app(fun("x", plus(constant(7), variable("x"))), constant(3))) == constant(10))
  }

  test("eval part B works") {
    assert(eval( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(eval(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(eval(iff(variable("x"), constant(3), constant(4)) ) == err("Conditional Error"))
    assert(eval(iff(fun("x", variable("x")), constant(3), constant(4))) == constant(3))
    assert(eval(iff(fun("x", variable("y")), constant(3), constant(4))) == constant(3))
  }/*pass*/

  test("eval part C  works") {
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
  }/*pass*/

  test("eval Y works") {
    assert(eval(app(app(Y, preFact), five)) == onetwenty)/*pass*/
  }
  /*Two calls to nextVar generate different values*/
  test("variable generator works") {assert (nextVar != nextVar) }/* pass!!*/

  //3b
  test("iscell works") {
    assert (eval(iscell(cell(constant(0), constant(1)))) == constant(1))
    assert (eval(iscell(constant(10))) == constant(0))
    assert (eval(iscell(fun("x", plus(variable("x"), constant(5))))) == constant(0))
  }

  test("sum works") {
    assert (eval(app(app(Y, preSum), constant(3))) == constant(6))
    assert (eval(app(app(Y, preSum), constant(4))) == constant(10))/*1+2+3+4*/
  }

}/*behaviourTest*/